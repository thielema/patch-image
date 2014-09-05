{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Option

import qualified Data.Array.Accelerate.Math.FFT as FFT
import qualified Data.Array.Accelerate.Math.Complex as Complex
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.IO as AIO
import qualified Data.Array.Accelerate.Arithmetic.LinearAlgebra as LinAlg
import qualified Data.Array.Accelerate.Utility.Lift.Run as Run
import qualified Data.Array.Accelerate.Utility.Lift.Exp as Exp
import qualified Data.Array.Accelerate.Utility.Arrange as Arrange
import qualified Data.Array.Accelerate.Utility.Loop as Loop
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Math.Complex (Complex((:+)), )
import Data.Array.Accelerate.Utility.Lift.Exp (atom)
import Data.Array.Accelerate
          (Acc, Array, Exp, DIM1, DIM2, DIM3,
           (:.)((:.)), Z(Z), Any(Any), All(All),
           (<*), (<=*), (>=*), (==*), (&&*), (||*), (?), )

import qualified Data.Packed.Matrix as Matrix
import qualified Data.Packed.Vector as Vector
import qualified Data.Packed.ST as PackST
import qualified Numeric.Container as Container
import Numeric.Container ((<\>), (<>))

import qualified Line
import Point2 (Point2(Point2), distance)

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV

import qualified System.FilePath as FilePath

import Text.Printf (printf)

import qualified Data.List.Key as Key
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Bits as Bit
import Control.Monad.HT (void)
import Control.Monad (liftM2, zipWithM_, when)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (catMaybes)
import Data.List.HT (removeEach, mapAdjacent, tails)
import Data.Traversable (forM)
import Data.Foldable (forM_, foldMap)
import Data.Tuple.HT (mapPair, mapFst, mapSnd, fst3, thd3)
import Data.Word (Word8)


readImage :: FilePath -> IO (Array DIM3 Word8)
readImage path = do
   epic <- Pic.readImage path
   case epic of
      Left msg -> ioError $ userError msg
      Right dynpic ->
         case dynpic of
            Pic.ImageYCbCr8 pic -> do
               let dat = Pic.imageData pic
               printf "yuv %dx%d, size %d\n"
                  (Pic.imageWidth pic)
                  (Pic.imageHeight pic)
                  (SV.length dat)
               return $
                  AIO.fromVectors
                     (Z :. Pic.imageHeight pic :. Pic.imageWidth pic :. 3)
                     ((), dat)
            _ -> ioError $ userError "unsupported image type"

writeImage :: Int -> FilePath -> Array DIM3 Word8 -> IO ()
writeImage quality path arr = do
   let (Z :. height :. width :. 3) = A.arrayShape arr
   Pic.saveJpgImage quality path $ Pic.ImageYCbCr8 $
      Pic.Image {
         Pic.imageWidth = width,
         Pic.imageHeight = height,
         Pic.imageData = snd $ AIO.toVectors arr
      }

writeGrey :: Int -> FilePath -> Array DIM2 Word8 -> IO ()
writeGrey quality path arr = do
   let (Z :. height :. width) = A.arrayShape arr
   Pic.saveJpgImage quality path $ Pic.ImageY8 $
      Pic.Image {
         Pic.imageWidth = width,
         Pic.imageHeight = height,
         Pic.imageData = snd $ AIO.toVectors arr
      }

imageFloatFromByte ::
   (A.Shape sh, A.Elt a, A.IsFloating a) =>
   Acc (Array sh Word8) -> Acc (Array sh a)
imageFloatFromByte = A.map ((/255) . A.fromIntegral)

imageByteFromFloat ::
   (A.Shape sh, A.Elt a, A.IsFloating a) =>
   Acc (Array sh a) -> Acc (Array sh Word8)
imageByteFromFloat = A.map (fastRound . (255*) . max 0 . min 1)


cycleLeftDim3 :: Exp DIM3 -> Exp DIM3
cycleLeftDim3 =
   Exp.modify (atom :. atom :. atom :. atom) $
       \(z :. chans :. height :. width) ->
          z :. height :. width :. chans

cycleRightDim3 :: Exp DIM3 -> Exp DIM3
cycleRightDim3 =
   Exp.modify (atom :. atom :. atom :. atom) $
       \(z :. height :. width :. chans) ->
          z :. chans :. height :. width

separateChannels :: (A.Elt a) => Acc (Array DIM3 a) -> Acc (Array DIM3 a)
separateChannels arr =
   A.backpermute
      (cycleRightDim3 $ A.shape arr)
      cycleLeftDim3
      arr

interleaveChannels :: (A.Elt a) => Acc (Array DIM3 a) -> Acc (Array DIM3 a)
interleaveChannels arr =
   A.backpermute
      (cycleLeftDim3 $ A.shape arr)
      cycleRightDim3
      arr


fastRound ::
   (A.Elt i, A.IsIntegral i, A.Elt a, A.IsFloating a) => Exp a -> Exp i
fastRound x = A.floor (x+0.5)

floatArray :: Acc (Array sh Float) -> Acc (Array sh Float)
floatArray = id


rotatePoint :: (Num a) => (a,a) -> (a,a) -> (a,a)
rotatePoint (c,s) (x,y) = (c*x-s*y, s*x+c*y)

rotateStretchMovePoint ::
   (Fractional a) =>
   (a, a) -> (a, a) ->
   (a, a) -> (a, a)
rotateStretchMovePoint rot (mx,my) p =
   mapPair ((mx+), (my+)) $ rotatePoint rot p

rotateStretchMoveBackPoint ::
   (Fractional a) =>
   (a, a) -> (a, a) ->
   (a, a) -> (a, a)
rotateStretchMoveBackPoint (rx,ry) (mx,my) =
   let corr = recip $ rx*rx + ry*ry
       rot = (corr*rx, -corr*ry)
   in  \(x,y) -> rotatePoint rot (x - mx, y - my)


boundingBoxOfRotated :: (Num a, Ord a) => (a,a) -> (a,a) -> ((a,a), (a,a))
boundingBoxOfRotated rot (w,h) =
   let (xs,ys) =
          unzip $
          rotatePoint rot (0,0) :
          rotatePoint rot (w,0) :
          rotatePoint rot (0,h) :
          rotatePoint rot (w,h) :
          []
   in  ((minimum xs, maximum xs), (minimum ys, maximum ys))

linearIp :: (Num a) => (a,a) -> a -> a
linearIp (x0,x1) t = (1-t) * x0 + t * x1

cubicIp :: (Fractional a) => (a,a,a,a) -> a -> a
cubicIp (xm1, x0, x1, x2) t =
   let lipm12 = linearIp (xm1,x2) t
       lip01  = linearIp (x0, x1) t
   in  lip01 + (t*(t-1)/2) * (lipm12 + (x0+x1) - 3 * lip01)

splitFraction :: (A.Elt a, A.IsFloating a) => Exp a -> (Exp Int, Exp a)
splitFraction x =
   let i = A.floor x
   in  (i, x - A.fromIntegral i)



type Channel ix a = Array (ix :. Int :. Int) a

type ExpDIM2 ix = Exp ix :. Exp Int :. Exp Int
type ExpDIM3 ix = Exp ix :. Exp Int :. Exp Int :. Exp Int

unliftDim2 ::
   (A.Slice ix) =>
   Exp (ix :. Int :. Int) -> ExpDIM2 ix
unliftDim2 = A.unlift


indexLimit ::
   (A.Slice ix, A.Shape ix, A.Elt a) =>
   Acc (Channel ix a) -> ExpDIM2 ix -> Exp a
indexLimit arr (ix:.y:.x) =
   let (_ :. height :. width) = unliftDim2 $ A.shape arr
       xc = max 0 $ min (width -1) x
       yc = max 0 $ min (height-1) y
   in  arr A.! A.lift (ix :. yc :. xc)

indexFrac ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   Acc (Channel ix a) -> Exp ix :. Exp a :. Exp a -> Exp a
indexFrac arr (ix:.y:.x) =
   let (xi,xf) = splitFraction x
       (yi,yf) = splitFraction y
       interpolRow yc =
          cubicIp
             (indexLimit arr (ix:.yc:.xi-1),
              indexLimit arr (ix:.yc:.xi  ),
              indexLimit arr (ix:.yc:.xi+1),
              indexLimit arr (ix:.yc:.xi+2))
             xf
   in  cubicIp
          (interpolRow (yi-1),
           interpolRow  yi,
           interpolRow (yi+1),
           interpolRow (yi+2))
          yf


rotateStretchMoveCoords ::
   (A.Elt a, A.IsFloating a) =>
   (Exp a, Exp a) ->
   (Exp a, Exp a) ->
   (Exp Int, Exp Int) ->
   Acc (Channel Z (a, a))
rotateStretchMoveCoords rot mov (width,height) =
   let trans = rotateStretchMoveBackPoint rot mov
   in  A.generate (A.lift $ Z:.height:.width) $ \p ->
          let (_z :. ydst :. xdst) = unliftDim2 p
          in  A.lift $ trans (A.fromIntegral xdst, A.fromIntegral ydst)

inBoxPlain ::
   (Ord a, Num a) =>
   (a, a) ->
   (a, a) ->
   Bool
inBoxPlain (width,height) (x,y) =
   0<=x && x<width && 0<=y && y<height

inBox ::
   (A.Elt a, A.IsNum a, A.IsScalar a) =>
   (Exp a, Exp a) ->
   (Exp a, Exp a) ->
   Exp Bool
inBox (width,height) (x,y) =
   0<=*x &&* x<*width &&* 0<=*y &&* y<*height

validCoords ::
   (A.Elt a, A.IsFloating a) =>
   (Exp Int, Exp Int) ->
   Acc (Channel Z (a, a)) ->
   Acc (Channel Z Bool)
validCoords (width,height) =
   A.map $ A.lift1 $ \(x,y) ->
      inBox (width,height) (fastRound x, fastRound y)

replicateChannel ::
   (A.Slice ix, A.Shape ix, A.Elt a) =>
   Exp ix -> Acc (Channel Z a) -> Acc (Channel ix a)
replicateChannel = LinAlg.extrudeMatrix

{- |
@rotateStretchMove rot mov@
first rotate and stretches the image according to 'rot'
and then moves the picture.
-}
rotateStretchMove ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   (Exp a, Exp a) ->
   (Exp a, Exp a) ->
   ExpDIM2 ix -> Acc (Channel ix a) ->
   (Acc (Channel Z Bool), Acc (Channel ix a))
rotateStretchMove rot mov sh arr =
   let ( chansDst :. heightDst :. widthDst) = sh
       (_chansSrc :. heightSrc :. widthSrc) = unliftDim2 $ A.shape arr
       coords = rotateStretchMoveCoords rot mov (widthDst, heightDst)

   in  (validCoords (widthSrc, heightSrc) coords,
        Arrange.mapWithIndex
           (\ix coord ->
              let (chan :. _ydst :. _xdst) = unliftDim2 ix
                  (xsrc,ysrc) = A.unlift coord
              in  indexFrac arr (chan :. ysrc :. xsrc))
           (replicateChannel chansDst coords))


rotate ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   (Exp a, Exp a) ->
   Acc (Channel ix a) -> Acc (Channel ix a)
rotate rot arr =
   let (chans :. height :. width) = unliftDim2 $ A.shape arr
       ((left, right), (top, bottom)) =
          boundingBoxOfRotated rot (A.fromIntegral width, A.fromIntegral height)
   in  snd $
       rotateStretchMove rot (-left,-top)
          (chans :. A.ceiling (bottom-top) :. A.ceiling (right-left)) arr


brightnessPlane ::
   (A.Slice ix, A.Shape ix) =>
   Acc (Channel (ix:.Int) Float) -> Acc (Channel ix Float)
brightnessPlane = flip A.slice (A.lift (Any :. (0::Int) :. All :. All))

rowHistogram :: Acc (Channel DIM1 Float) -> Acc (Array DIM1 Float)
rowHistogram = A.fold (+) 0 . brightnessPlane


rotateHistogram ::
   Float -> Array DIM3 Word8 -> (Array DIM3 Word8, Array DIM1 Float)
rotateHistogram =
   let rot =
          Run.with CUDA.run1 $ \orient arr ->
             let rotated =
                    rotate orient $
                    separateChannels $ imageFloatFromByte arr
             in  (imageByteFromFloat $ interleaveChannels rotated,
                  rowHistogram rotated)
   in  \angle arr -> rot (cos angle, sin angle) arr


analyseRotations :: Array DIM3 Word8 -> IO ()
analyseRotations pic = do
   histograms <-
      forM [-100,-95..100::Int] $ \angle -> do
         let degree = fromIntegral angle / 100
         let (rotated, histogram) = rotateHistogram (degree * pi/180) pic
         let stem = printf "rotated%+07.2f" degree
         writeImage 90 ("/tmp/" ++ stem ++ ".jpeg") rotated
         let diffHistogram = map abs $ mapAdjacent (-) $ A.toList histogram
         printf "%s: maxdiff %8.3f, sqrdiff %8.0f\n"
            stem (maximum diffHistogram) (sum $ map (^(2::Int)) diffHistogram)
         return (stem, histogram)
   void $ GP.plotDefault $
      foldMap
         (\(label, histogram) ->
            fmap (Graph2D.lineSpec (LineSpec.title label LineSpec.deflt)) $
            Plot2D.list Graph2D.listLines $ A.toList histogram)
         histograms
   void $ GP.plotDefault $
      foldMap
         (\(label, histogram) ->
            fmap (Graph2D.lineSpec (LineSpec.title label LineSpec.deflt)) $
            Plot2D.list Graph2D.listLines $
            map abs $ mapAdjacent (-) $ A.toList histogram)
         histograms



differentiate ::
   (A.Elt a, A.IsNum a) =>
   Acc (Array DIM1 a) -> Acc (Array DIM1 a)
differentiate arr =
   let size = A.unindex1 $ A.shape arr
   in  A.generate (A.index1 (size-1)) $ \i ->
          arr A.! (A.index1 $ A.unindex1 i + 1) - arr A.! i

scoreRotation :: Float -> Array DIM3 Word8 -> Float
scoreRotation =
   let rot =
          Run.with CUDA.run1 $ \orient arr ->
             A.sum $ A.map (^(2::Int)) $ differentiate $ rowHistogram $
             rotate orient $ separateChannels $ imageFloatFromByte arr
   in  \angle arr -> A.indexArray (rot (cos angle, sin angle) arr) Z

findOptimalRotation :: Array DIM3 Word8 -> Float
findOptimalRotation pic =
   Key.maximum (flip scoreRotation pic . (* (pi/180))) $
   map (\a -> fromIntegral a / 100) [-100,-95..100::Int]





rotateManifest :: Float -> Array DIM3 Word8 -> Array DIM3 Float
rotateManifest =
   let rot =
          Run.with CUDA.run1 $ \orient arr ->
             rotate orient $ separateChannels $ imageFloatFromByte arr
   in  \angle arr -> rot (cos angle, sin angle) arr


prepareOverlapMatching ::
   Int -> (Float, Array DIM3 Word8) -> Channel Z Float
prepareOverlapMatching =
   let rot =
          Run.with CUDA.run1 $ \radius orient arr ->
             rotate orient $
             (if True
                then highpass radius
                else removeDCOffset) $
             brightnessPlane $ separateChannels $ imageFloatFromByte arr
   in  \radius (angle, arr) -> rot radius (cos angle, sin angle) arr


ceilingPow2Exp :: Exp Int -> Exp Int
ceilingPow2Exp n =
   A.setBit 0 $ A.ceiling $ logBase 2 (fromIntegral n :: Exp Double)

pad ::
   (A.Elt a) =>
   Exp a -> Exp DIM2 -> Acc (Channel Z a) -> Acc (Channel Z a)
pad a sh arr =
   let (height, width) = A.unlift $ A.unindex2 $ A.shape arr
   in  A.generate sh $ \p ->
          let (y, x) = A.unlift $ A.unindex2 p
          in  (y<*height &&* x<*width)
              ?
              (arr A.! A.index2 y x, a)

convolveImpossible ::
   (A.Elt a, A.IsFloating a) =>
   Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
convolveImpossible x y =
   let (heightx, widthx) = A.unlift $ A.unindex2 $ A.shape x
       (heighty, widthy) = A.unlift $ A.unindex2 $ A.shape y
       width  = ceilingPow2Exp $ widthx  + widthy
       height = ceilingPow2Exp $ heightx + heighty
       sh = A.index2 height width
       forward z =
          FFT.fft2D FFT.Forward $ CUDA.run $
          A.map (A.lift . (:+ 0)) $ pad 0 sh z
   in  A.map Complex.real $
       FFT.fft2D FFT.Inverse $ CUDA.run $
       A.zipWith (*) (forward x) (forward y)


ceilingPow2 :: Int -> Int
ceilingPow2 n =
   Bit.setBit 0 $ ceiling $ logBase 2 (fromIntegral n :: Double)

removeDCOffset ::
   (A.Elt a, A.IsFloating a) => Acc (Channel Z a) -> Acc (Channel Z a)
removeDCOffset arr =
   let sh = A.shape arr
       (_z :. height :. width) = unliftDim2 sh
       s =
          A.the (A.fold1All (+) arr)
             / (A.fromIntegral width * A.fromIntegral height)
   in  A.map (subtract s) arr

{-
We cannot remove DC offset in the spectrum,
because we already padded the images with zeros.
-}
clearDCCoefficient ::
   (A.Elt a, A.IsFloating a) =>
   Acc (Array DIM2 (Complex a)) -> Acc (Array DIM2 (Complex a))
clearDCCoefficient arr =
   A.generate (A.shape arr) $ \p ->
      let (_z:.y:.x) = unliftDim2 p
      in  x==*0 ||* y==*0 ? (0, arr A.! p)


smooth3 :: (A.Elt a, A.IsFloating a) => A.Stencil3 a -> Exp a
smooth3 (l,m,r) = (l+2*m+r)/4

lowpass, highpass ::
   (A.Elt a, A.IsFloating a) =>
   Exp Int -> Acc (Channel Z a) -> Acc (Channel Z a)
lowpass count =
   Loop.nest count $
      A.stencil (\(a,m,b) -> smooth3 (smooth3 a, smooth3 m, smooth3 b)) A.Clamp

highpass count arr =
  A.zipWith (-) arr $ lowpass count arr


convolvePaddedSimple ::
   (A.Elt a, A.IsFloating a) =>
   DIM2 -> Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
convolvePaddedSimple sh@(Z :. height :. width) =
   let forward =
          FFT.fft2D' FFT.Forward width height .
          A.map (A.lift . (:+ 0)) . pad 0 (A.lift sh)
       inverse = FFT.fft2D' FFT.Inverse width height
   in  \ x y ->
          A.map Complex.real $ inverse $
          A.zipWith (\xi yi -> xi * Complex.conj yi) (forward x) (forward y)


imagUnit :: (A.Elt a, A.IsNum a) => Exp (Complex a)
imagUnit = Exp.modify2 atom atom (:+) 0 1

{- |
Let f and g be two real valued images.
The spectrum of f+i*g is spec f + i * spec g.
Let 'flip' be the spectrum with negated indices modulo image size.
It holds: flip (spec f) = conj (spec f).

(a + conj b) / 2
  = (spec (f+i*g) + conj (flip (spec (f+i*g)))) / 2
  = (spec f + i*spec g + conj (flip (spec f)) + conj (flip (spec (i*g)))) / 2
  = (2*spec f + i*spec g + conj (i*flip (spec g))) / 2
  = (2*spec f + i*spec g - i * conj (flip (spec g))) / 2
  = spec f

(a - conj b) * (-i/2)
  = (-i*a + conj (-i*b)) / 2
  -> this swaps role of f and g in the proof above
-}
untangleRealSpectra ::
   (A.Elt a, A.IsFloating a) =>
   Acc (Array DIM2 (Complex a)) -> Acc (Array DIM2 (Complex a, Complex a))
untangleRealSpectra spec =
   A.zipWith
      (\a b ->
         A.lift $
            ((a + Complex.conj b) / 2,
             (a - Complex.conj b) * (-imagUnit / 2)))
      spec $
   A.backpermute (A.shape spec)
      (Exp.modify (atom:.atom:.atom) $
       \(_z:.y:.x) ->
          let (_z:.height:.width) = unliftDim2 $ A.shape spec
          in  Z :. mod (-y) height :. mod (-x) width)
      spec

{-
This is more efficient than 'convolvePaddedSimple'
since it needs only one forward Fourier transform,
where 'convolvePaddedSimple' needs two of them.
For the analysis part,
perform two real-valued Fourier transforms using one complex-valued transform.
Afterwards we untangle the superposed spectra.
-}
convolvePadded ::
   (A.Elt a, A.IsFloating a) =>
   DIM2 -> Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
convolvePadded sh@(Z :. height :. width) =
   let forward = FFT.fft2D' FFT.Forward width height
       inverse = FFT.fft2D' FFT.Inverse width height
   in  \ a b ->
          A.map Complex.real $ inverse $
          A.map (Exp.modify (atom,atom) $ \(ai,bi) -> ai * Complex.conj bi) $
          untangleRealSpectra $ forward $
          pad 0 (A.lift sh) $
          A.zipWith (Exp.modify2 atom atom (:+)) a b


attachDisplacements ::
   (A.Elt a, A.IsScalar a) =>
   Exp Int -> Exp Int ->
   Acc (Channel Z a) -> Acc (Channel Z ((Int, Int), a))
attachDisplacements xsplit ysplit arr =
   let sh = A.shape arr
       (_z :. height :. width) = unliftDim2 sh
   in  A.generate sh $ \p ->
          let (_z:.y:.x) = unliftDim2 p
              wrap size split c = c<*split ? (c, c-size)
          in  A.lift ((wrap width xsplit x, wrap height ysplit y), arr A.! p)

weightOverlapScores ::
   (A.Elt a, A.IsFloating a, A.IsScalar a) =>
   Exp Int -> (Exp Int, Exp Int) -> (Exp Int, Exp Int) ->
   Acc (Channel Z ((Int, Int), a)) ->
   Acc (Channel Z ((Int, Int), a))
weightOverlapScores minOverlap (widtha,heighta) (widthb,heightb) =
   A.map
       (Exp.modify ((atom,atom),atom) $ \(dp@(dy,dx),v) ->
          let clipWidth  = min widtha  (widthb  + dx) - max 0 dx
              clipHeight = min heighta (heightb + dy) - max 0 dy
          in  (dp,
                 (clipWidth >=* minOverlap  &&*  clipHeight >=* minOverlap)
                 ?
                 (v / (A.fromIntegral clipWidth * A.fromIntegral clipHeight), 0)))

{- |
Set all scores to zero within a certain border.
Otherwise the matching algorithm will try to match strong bars at the borders
that are actually digitalization artifacts.
-}
minimumOverlapScores ::
   (A.Elt a, A.IsFloating a, A.IsScalar a) =>
   Exp Int -> (Exp Int, Exp Int) -> (Exp Int, Exp Int) ->
   Acc (Channel Z ((Int, Int), a)) ->
   Acc (Channel Z ((Int, Int), a))
minimumOverlapScores minOverlap (widtha,heighta) (widthb,heightb) =
   A.map
       (Exp.modify ((atom,atom),atom) $ \(dp@(dy,dx),v) ->
          let clipWidth  = min widtha  (widthb  + dx) - max 0 dx
              clipHeight = min heighta (heightb + dy) - max 0 dy
          in  (dp,
                 (clipWidth >=* minOverlap  &&*  clipHeight >=* minOverlap)
                 ?
                 (v, 0)))

argmax ::
   (A.Elt a, A.Elt b, A.IsScalar b) =>
   Exp (a, b) -> Exp (a, b) -> Exp (a, b)
argmax x y  =  A.snd x <* A.snd y ? (y,x)

argmaximum ::
   (A.Elt a, A.Elt b, A.IsScalar b) =>
   Acc (Channel Z (a, b)) -> Acc (A.Scalar (a, b))
argmaximum = A.fold1All argmax


allOverlaps ::
   DIM2 ->
   Exp Float ->
   Acc (Channel Z Float) -> Acc (Channel Z Float) ->
   Acc (Channel Z ((Int, Int), Float))
allOverlaps size@(Z :. height :. width) minOverlapPortion =
   let convolve = convolvePadded size
   in  \a b ->
          let (Z :. heighta :. widtha) = A.unlift $ A.shape a
              (Z :. heightb :. widthb) = A.unlift $ A.shape b
              half = flip div 2
              minOverlap =
                 fastRound $
                    minOverlapPortion
                    *
                    A.fromIntegral
                       (min
                          (min widtha heighta)
                          (min widthb heightb))
              weight =
                 if False
                   then
                      weightOverlapScores minOverlap
                         (widtha, heighta)
                         (widthb, heightb)
                   else
                      minimumOverlapScores minOverlap
                         (widtha, heighta)
                         (widthb, heightb)
          in  weight $
              attachDisplacements
                 (half $ A.lift width - widthb + widtha)
                 (half $ A.lift height - heightb + heighta) $
              convolve a b


allOverlapsRun ::
   DIM2 -> Float -> Channel Z Float -> Channel Z Float -> Channel Z Word8
allOverlapsRun padExtent =
   Run.with CUDA.run1 $ \minOverlap picA picB ->
      imageByteFromFloat $
      -- A.map (2*) $
      A.map (0.0001*) $
      A.map A.snd $ allOverlaps padExtent minOverlap picA picB

optimalOverlap ::
   DIM2 -> Float -> Channel Z Float -> Channel Z Float -> ((Int, Int), Float)
optimalOverlap padExtent =
   let run =
          Run.with CUDA.run1 $ \minimumOverlap a b ->
          argmaximum $ allOverlaps padExtent minimumOverlap a b
   in  \overlap a b -> A.indexArray (run overlap a b) Z


shrink ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   ExpDIM2 Z -> Acc (Channel ix a) -> Acc (Channel ix a)
shrink (_:.yk:.xk) arr =
   let (shape:.height:.width) = unliftDim2 $ A.shape arr
   in  A.map (/ (A.fromIntegral xk * A.fromIntegral yk)) $
       A.fold1 (+) $ A.fold1 (+) $
       A.backpermute
          (A.lift $ shape :. div height yk :. div width xk :. yk :. xk)
          (Exp.modify (atom:.atom:.atom:.atom:.atom) $
           \(z:.yi:.xi:.yj:.xj) -> z:.yi*yk+yj:.xi*xk+xj)
          arr

-- cf. numeric-prelude
divUp :: (Integral a) => a -> a -> a
divUp a b = - div (-a) b

{-
Reduce image sizes below the padExtent before matching images.
-}
optimalOverlapBig ::
   DIM2 -> Float -> Channel Z Float -> Channel Z Float -> ((Int, Int), Float)
optimalOverlapBig padExtent@(Z:.heightPad:.widthPad) =
   let run =
          Run.with CUDA.run1 $ \minimumOverlap a b ->
             let (Z :. heighta :. widtha) = A.unlift $ A.shape a
                 (Z :. heightb :. widthb) = A.unlift $ A.shape b
                 yk = divUp (heighta+heightb) $ A.lift heightPad
                 xk = divUp (widtha +widthb)  $ A.lift widthPad
                 factors = A.lift Z :. yk :. xk
                 scalePos =
                    Exp.modify ((atom,atom), atom) $
                    \((xm,ym), score) -> ((xm*xk, ym*yk), score)
             in  A.map scalePos $ argmaximum $
                 allOverlaps padExtent minimumOverlap
                    (shrink factors a) (shrink factors b)
   in  \minimumOverlap a b -> A.indexArray (run minimumOverlap a b) Z


clip ::
   (A.Slice ix, A.Shape ix, A.Elt a) =>
   (Exp Int, Exp Int) ->
   (Exp Int, Exp Int) ->
   Acc (Channel ix a) -> Acc (Channel ix a)
clip (left,top) (width,height) arr =
   A.backpermute
      (A.lift $ A.indexTail (A.indexTail (A.shape arr)) :. height :. width)
      (Exp.modify (atom:.atom:.atom) $
       \(z :. y :. x) -> z :. y+top :. x+left)
      arr

{-
Like 'optimalOverlapBig'
but computes precise distance in a second step
using a part in the overlapping area..
-}
optimalOverlapBigFine ::
   DIM2 -> Float -> Channel Z Float -> Channel Z Float -> ((Int, Int), Float)
optimalOverlapBigFine padExtent@(Z:.heightPad:.widthPad) =
   let run =
          Run.with CUDA.run1 $ \minimumOverlap a b ->
             let (Z :. heighta :. widtha) = A.unlift $ A.shape a
                 (Z :. heightb :. widthb) = A.unlift $ A.shape b
                 yk = divUp (heighta+heightb) $ A.lift heightPad
                 xk = divUp (widtha +widthb)  $ A.lift widthPad
                 factors = A.lift Z :. yk :. xk
                 (coarsedx,coarsedy) =
                    mapPair ((xk*), (yk*)) $
                    Exp.unliftPair $ A.fst $ A.the $ argmaximum $
                    allOverlaps padExtent minimumOverlap
                       (shrink factors a) (shrink factors b)
                 leftOverlap = max 0 coarsedx
                 topOverlap  = max 0 coarsedy
                 rightOverlap  = min widtha  (widthb  + coarsedx)
                 bottomOverlap = min heighta (heightb + coarsedy)
                 widthOverlap  = rightOverlap - leftOverlap
                 heightOverlap = bottomOverlap - topOverlap
                 widthFocus  = min widthOverlap $ A.lift $ div widthPad 2
                 heightFocus = min heightOverlap $ A.lift $ div heightPad 2
                 extentFocus = (widthFocus,heightFocus)
                 leftFocus = leftOverlap + div (widthOverlap-widthFocus) 2
                 topFocus  = topOverlap  + div (heightOverlap-heightFocus) 2
                 addCoarsePos =
                    Exp.modify ((atom,atom), atom) $
                    \((xm,ym), score) -> ((xm+coarsedx, ym+coarsedy), score)
             in  A.map addCoarsePos $ argmaximum $
                 allOverlaps padExtent minimumOverlap
                    (clip (leftFocus,topFocus) extentFocus a)
                    (clip (leftFocus-coarsedx,topFocus-coarsedy) extentFocus b)
   in  \minimumOverlap a b -> A.indexArray (run minimumOverlap a b) Z


overlapDifference ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   (Exp Int, Exp Int) ->
   Acc (Channel ix a) -> Acc (Channel ix a) -> Acc (A.Scalar a)
overlapDifference (dx,dy) a b =
   let (_ :. heighta :. widtha) = unliftDim2 $ A.shape a
       (_ :. heightb :. widthb) = unliftDim2 $ A.shape b
       leftOverlap = max 0 dx
       topOverlap  = max 0 dy
       rightOverlap  = min widtha  (widthb  + dx)
       bottomOverlap = min heighta (heightb + dy)
       widthOverlap  = rightOverlap - leftOverlap
       heightOverlap = bottomOverlap - topOverlap
       extentOverlap = (widthOverlap,heightOverlap)
   in  A.map sqrt $
       A.map (/(A.fromIntegral widthOverlap * A.fromIntegral heightOverlap)) $
       A.fold1All (+) $
       A.map (^(2::Int)) $
       A.zipWith (-)
          (clip (leftOverlap,topOverlap) extentOverlap a)
          (clip (leftOverlap-dx,topOverlap-dy) extentOverlap b)

overlapDifferenceRun ::
   (Int, Int) ->
   Channel Z Float -> Channel Z Float -> Float
overlapDifferenceRun =
   let diff = Run.with CUDA.run1 overlapDifference
   in  \d a b -> A.indexArray (diff d a b) Z


absolutePositionsFromPairDisplacements ::
   Int -> [((Int, Int), (Int, Int))] ->
   ([(Double,Double)], [(Double,Double)])
absolutePositionsFromPairDisplacements numPics displacements =
   let (is, ds) = unzip displacements
       (dxs, dys) = unzip ds
       {-
       We fix the first image to position (0,0)
       in order to make the solution unique.
       To this end I drop the first column from matrix.
       -}
       matrix = Matrix.dropColumns 1 $ PackST.runSTMatrix $ do
          mat <- PackST.newMatrix 0 (length is) numPics
          zipWithM_
             (\k (ia,ib) -> do
                PackST.writeMatrix mat k ia (-1)
                PackST.writeMatrix mat k ib 1)
             [0..] is
          return mat
       pxs = matrix <\> Vector.fromList (map fromIntegral dxs)
       pys = matrix <\> Vector.fromList (map fromIntegral dys)
   in  (zip (0 : Vector.toList pxs) (0 : Vector.toList pys),
        zip (Vector.toList $ matrix <> pxs) (Vector.toList $ matrix <> pys))

{-
Approximate rotation from point correspondences.
Here (dx, dy) is the displacement with respect to the origin (0,0),
that is, the pair plays the role of the absolute position.

x1 = dx + c*x0 - s*y0
y1 = dy + s*x0 + c*y0

               /dx\
/1 0 x0 -y0\ . |dy| = /x1\
\0 1 y0  x0/   |c |   \y1/
               \s /

Maybe, dx and dy should be scaled down.
Otherwise they are weighted much more than the rotation.
-}
layoutFromPairDisplacements ::
   Int -> [((Int, (Int, Int)), (Int, (Int, Int)))] ->
   ([((Double,Double), (Double,Double))],
    [(Double,Double)])
layoutFromPairDisplacements numPics correspondences =
   let {-
       (ca, cb) = unzip correspondences
       (is, ds) = unzip correspondences
       (dxs, dys) = unzip ds
       -}
       {-
       We fix the first image to position (0,0)
       in order to make the solution unique.
       To this end I drop the first column from matrix.
       -}
       matrix = Matrix.dropColumns 4 $ PackST.runSTMatrix $ do
          mat <- PackST.newMatrix 0 (2 * length correspondences) numPics
          zipWithM_
             (\k ((ia,(xai,yai)),(ib,(xbi,ybi))) -> do
                let xa = fromIntegral xai
                let xb = fromIntegral xbi
                let ya = fromIntegral yai
                let yb = fromIntegral ybi
                PackST.writeMatrix mat (k+0) (4*ia+0) (-1)
                PackST.writeMatrix mat (k+1) (4*ia+1) (-1)
                PackST.writeMatrix mat (k+0) (4*ia+2) xa
                PackST.writeMatrix mat (k+0) (4*ia+3) (-ya)
                PackST.writeMatrix mat (k+1) (4*ia+2) ya
                PackST.writeMatrix mat (k+1) (4*ia+3) xa
                PackST.writeMatrix mat (k+0) (4*ib+0) 1
                PackST.writeMatrix mat (k+1) (4*ib+1) 1
                PackST.writeMatrix mat (k+0) (4*ib+2) xb
                PackST.writeMatrix mat (k+0) (4*ib+3) (-yb)
                PackST.writeMatrix mat (k+1) (4*ib+2) yb
                PackST.writeMatrix mat (k+1) (4*ib+3) xb)
             [0,2..] correspondences
          return mat
       ps =
          matrix
          <\>
          Container.constant 0 (2 * length correspondences)
   in  (map (\[dx,dy,rx,ry] -> ((dx,dy), (rx,ry))) $
        ListHT.sliceVertical 4 $ Vector.toList ps,
        map (\[dx,dy] -> (dx,dy)) $
        ListHT.sliceVertical 2 $ Vector.toList $ matrix <> ps)


overlap2 ::
   (A.Slice ix, A.Shape ix) =>
   (Exp Int, Exp Int) ->
   (Acc (Channel ix Float), Acc (Channel ix Float)) -> Acc (Channel ix Float)
overlap2 (dx,dy) (a,b) =
   let (chansa :. heighta :. widtha) = unliftDim2 $ A.shape a
       (chansb :. heightb :. widthb) = unliftDim2 $ A.shape b
       left = min 0 dx; right  = max widtha  (widthb  + dx)
       top  = min 0 dy; bottom = max heighta (heightb + dy)
       width  = right - left
       height = bottom - top
       chans = A.intersect chansa chansb
   in  A.generate (A.lift (chans :. height :. width)) $ \p ->
          let (chan :. y :. x) = unliftDim2 p
              xa = x + left; xb = xa-dx
              ya = y + top;  yb = ya-dy
              pa = A.lift $ chan :. ya :. xa
              pb = A.lift $ chan :. yb :. xb
              inPicA = 0<=*xa &&* xa<*widtha &&* 0<=*ya &&* ya<*heighta
              inPicB = 0<=*xb &&* xb<*widthb &&* 0<=*yb &&* yb<*heightb
          in  inPicA ?
                 (inPicB ? ((a A.! pa + b A.! pb)/2, a A.! pa),
                  inPicB ? (b A.! pb, 0))

composeOverlap ::
   (Int, Int) ->
   ((Float, Array DIM3 Word8), (Float, Array DIM3 Word8)) ->
   Array DIM3 Word8
composeOverlap =
   let rot (angle,pic) =
          rotate (cos angle, sin angle) $
          separateChannels $ imageFloatFromByte pic
   in  Run.with CUDA.run1 $
       \(dx,dy) (a,b) ->
          imageByteFromFloat $ interleaveChannels $
          overlap2 (dx, dy) (rot a, rot b)


emptyCanvas ::
   (A.Slice ix, A.Shape ix) =>
   ix :. Int :. Int ->
   (Channel Z Int, Channel ix Float)
emptyCanvas =
   Run.with CUDA.run1 $ \sh ->
      let (_ix :. height :. width) = unliftDim2 sh
      in  (A.fill (A.lift $ Z:.height:.width) 0,
           A.fill sh 0)


addToCanvas ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsNum a) =>
   (Acc (Channel Z Bool), Acc (Channel ix a)) ->
   (Acc (Channel Z Int),  Acc (Channel ix a)) ->
   (Acc (Channel Z Int),  Acc (Channel ix a))
addToCanvas (mask, pic) (count, canvas) =
   (A.zipWith (+) (A.map A.boolToInt mask) count,
    A.zipWith (+) canvas $ A.zipWith (*) pic $
    replicateChannel
       (A.indexTail $ A.indexTail $ A.shape pic)
       (A.map (A.fromIntegral . A.boolToInt) mask))

updateCanvas ::
   (Float,Float) -> (Float,Float) -> Array DIM3 Word8 ->
   (Channel Z Int, Channel DIM1 Float) ->
   (Channel Z Int, Channel DIM1 Float)
updateCanvas =
   Run.with CUDA.run1 $
   \rot mov pic (count,canvas) ->
      addToCanvas
         (rotateStretchMove rot mov (unliftDim2 $ A.shape canvas) $
          separateChannels $ imageFloatFromByte pic)
         (count,canvas)

finalizeCanvas :: (Channel Z Int, Channel DIM1 Float) -> Array DIM3 Word8
finalizeCanvas =
   Run.with CUDA.run1 $
   \(count, canvas) ->
      imageByteFromFloat $ interleaveChannels $
      A.zipWith (/) canvas $
      replicateChannel (A.indexTail $ A.indexTail $ A.shape canvas) $
      A.map A.fromIntegral count



maybePlus ::
   (A.Elt a) =>
   (Exp a -> Exp a -> Exp a) ->
   Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)
maybePlus f x y =
   let (xb,xv) = Exp.unliftPair x
       (yb,yv) = Exp.unliftPair y
   in  A.cond xb (A.lift (True, A.cond yb (f xv yv) xv)) y

maskedMinimum ::
   (A.Shape ix, A.Elt a, A.IsScalar a) =>
   LinAlg.Vector ix (Bool, a) ->
   LinAlg.Scalar ix (Bool, a)
maskedMinimum = A.fold1 (maybePlus min)

maskedMaximum ::
   (A.Shape ix, A.Elt a, A.IsScalar a) =>
   LinAlg.Vector ix (Bool, a) ->
   LinAlg.Scalar ix (Bool, a)
maskedMaximum = A.fold1 (maybePlus max)



intersections ::
   (Fractional a, Ord a) =>
   [Line.L2 a] -> [Line.L2 a] -> [Point2 a]
intersections segments0 segments1 =
   catMaybes $ liftM2 Line.intersect segments0 segments1

project ::
   (A.Elt a, A.IsFloating a) =>
   Point2 (Exp a) ->
   (Point2 (Exp a), Point2 (Exp a)) ->
   (Exp Bool, Point2 (Exp a))
project x (a, b) =
   let (r, _, _, y) = Line.distanceAux a b x
   in  (0<=*r &&* r<=*1, y)

distanceMapEdges ::
   (A.Elt a, A.IsFloating a) =>
   Exp DIM2 -> Acc (Array DIM1 ((a,a),(a,a))) -> Acc (Channel Z a)
distanceMapEdges sh edges =
   A.map (Exp.modify (atom,atom) $ \(valid, dist) -> valid ? (dist, 0)) $
   maskedMinimum $
   outerVector
      (Exp.modify2 (atom,atom) ((atom, atom), (atom, atom)) $ \p (q0, q1) ->
         let pp = Point2 p
         in  mapSnd (distance pp) $ project pp (Point2 q0, Point2 q1))
      (pixelCoordinates sh)
      edges

distanceMapEdgesRun ::
   DIM2 -> Array DIM1 ((Float,Float),(Float,Float)) -> Channel Z Word8
distanceMapEdgesRun =
   Run.with CUDA.run1 $ \sh ->
      imageByteFromFloat . A.map (0.01*) . distanceMapEdges sh

distanceMapBox ::
   (A.Elt a, A.IsFloating a) =>
   Exp DIM2 ->
   Exp ((a,a), (a,a), (Int,Int)) ->
   Acc (Channel Z (Bool, (((a,(a,a)), (a,(a,a))), ((a,(a,a)), (a,(a,a))))))
distanceMapBox sh geom =
   let (rot, mov, extent@(width,height)) =
          Exp.unlift ((atom,atom),(atom,atom),(atom,atom)) geom
       widthf  = A.fromIntegral width
       heightf = A.fromIntegral height
       back  = rotateStretchMoveBackPoint rot mov
       forth = rotateStretchMovePoint rot mov
   in  A.generate sh $ \p ->
          let _z:.y:.x = unliftDim2 p
              (xsrc,ysrc) = back (A.fromIntegral x, A.fromIntegral y)
              leftDist = max 0 xsrc
              rightDist = max 0 $ widthf - xsrc
              topDist = max 0 ysrc
              bottomDist = max 0 $ heightf - ysrc
          in  A.lift $
              (inBox extent (fastRound xsrc, fastRound ysrc),
               (((leftDist, forth (0,ysrc)),
                 (rightDist, forth (widthf,ysrc))),
                ((topDist, forth (xsrc,0)),
                 (bottomDist, forth (xsrc,heightf)))))


-- cf. Data.Array.Accelerate.Arithmetic.Interpolation
outerVector ::
   (A.Shape ix, A.Slice ix, A.Elt a, A.Elt b, A.Elt c) =>
   (Exp a -> Exp b -> Exp c) ->
   LinAlg.Scalar ix a -> LinAlg.Vector Z b -> LinAlg.Vector ix c
outerVector f x y =
   A.zipWith f
      (A.replicate (A.lift $ Any :. LinAlg.numElems y) x)
      (LinAlg.extrudeVector (A.shape x) y)

separateDistanceMap ::
   (A.Elt a) =>
   Acc (Channel Z (Bool, ((a, a), (a, a)))) ->
   Acc (Array DIM3 (Bool, a))
separateDistanceMap arr =
   outerVector
      (Exp.modify2 (atom, ((atom, atom), (atom, atom))) (atom,atom) $
       \(b,(horiz,vert)) (orient,side) ->
          (b, orient ? (side ? horiz, side ? vert)))
      arr
      (A.use $ A.fromList (Z:.(4::Int)) $
       liftM2 (,) [False,True] [False,True])

distanceMapBoxRun ::
   DIM2 -> ((Float,Float),(Float,Float),(Int,Int)) -> Channel Z Word8
distanceMapBoxRun =
   Run.with CUDA.run1 $ \sh geom ->
      let scale =
             (4/) $ A.fromIntegral $ uncurry min $
             Exp.unliftPair $ Exp.thd3 geom
      in  imageByteFromFloat $
          A.map (Exp.modify (atom,atom) $
                   \(valid, dist) -> valid ? (scale*dist, 0)) $
          maskedMinimum $
          A.map (Exp.mapSnd A.fst) $
          separateDistanceMap $
          distanceMapBox sh geom


-- maybe move to Utility
{- |
We use it as a work-around.
Fusion of 'fold1' and 'replicate' would be very welcome
but it seems to fail with current accelerate version.
-}
breakFusion :: (A.Arrays a) => Acc a -> Acc a
breakFusion = id A.>-> id

containedAnywhere ::
   (A.Elt a, A.IsFloating a) =>
   Acc (Array DIM1 ((a,a), (a,a), (Int,Int))) ->
   Acc (Array DIM3 (a,a)) ->
   Acc (Array DIM3 Bool)
containedAnywhere geoms arr =
   A.fold1 (||*) $
   breakFusion $
   outerVector
      (Exp.modify2 (atom,atom) ((atom,atom),(atom,atom),(atom,atom)) $
       \(xdst,ydst) (rot, mov, extent) ->
          let (xsrc,ysrc) = rotateStretchMoveBackPoint rot mov (xdst,ydst)
          in  inBox extent (fastRound xsrc, fastRound ysrc))
      arr geoms


distanceMapContained ::
   (A.IsFloating a, A.Elt a) =>
   Exp DIM2 ->
   Exp ((a, a), (a, a), (Int, Int)) ->
   Acc (Array DIM1 ((a, a), (a, a), (Int, Int))) ->
   Acc (Channel Z a)
distanceMapContained sh this others =
   let distMap =
          separateDistanceMap $
          distanceMapBox sh this
       contained =
          containedAnywhere others $
          A.map (A.snd . A.snd) distMap
   in  A.map (Exp.modify (atom,atom) $
                \(valid, dist) -> valid ? (dist, 0)) $
       maskedMinimum $
       A.zipWith
          (Exp.modify2 atom (atom,(atom,atom)) $ \c (b,(dist,_)) ->
             (c&&*b, dist))
          contained distMap

distanceMapContainedRun ::
   DIM2 ->
   ((Float,Float),(Float,Float),(Int,Int)) ->
   [((Float,Float),(Float,Float),(Int,Int))] ->
   Channel Z Word8
distanceMapContainedRun =
   let distances =
          Run.with CUDA.run1 $
          \sh this others ->
             let scale =
                    (4/) $ A.fromIntegral $ uncurry min $
                    Exp.unliftPair $ Exp.thd3 this
             in  imageByteFromFloat $ A.map (scale*) $
                 distanceMapContained sh this others
   in  \sh this others ->
          distances sh this
             (A.fromList (Z :. length others) others)


pixelCoordinates ::
   (A.Elt a, A.IsFloating a) =>
   Exp DIM2 -> Acc (Channel Z (a,a))
pixelCoordinates sh =
   A.generate sh $ Exp.modify (atom:.atom:.atom) $ \(_z:.y:.x) ->
      (A.fromIntegral x, A.fromIntegral y)

distanceMapPoints ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   Acc (Array ix (a,a)) ->
   Acc (Array DIM1 (a,a)) ->
   Acc (Array ix a)
distanceMapPoints a b =
   A.fold1 min $
   outerVector
      (Exp.modify2 (atom,atom) (atom,atom) $
       \pa pb -> distance (Point2 pa) (Point2 pb))
      a b

distanceMapPointsRun ::
   DIM2 ->
   [Point2 Float] ->
   Channel Z Word8
distanceMapPointsRun =
   let distances =
          Run.with CUDA.run1 $
          \sh points ->
             let scale =
                    case Exp.unlift (atom:.atom:.atom) sh of
                       _z:.y:.x -> (4/) $ A.fromIntegral $ min x y
             in  imageByteFromFloat $ A.map (scale*) $
                 distanceMapPoints (pixelCoordinates sh) points
   in  \sh points ->
          distances sh $
             A.fromList (Z :. length points) $
             map (\(Point2 p) -> p) points


distanceMap ::
   (A.Elt a, A.IsFloating a) =>
   Exp DIM2 ->
   Exp ((a, a), (a, a), (Int, Int)) ->
   Acc (Array DIM1 ((a, a), (a, a), (Int, Int))) ->
   Acc (Array DIM1 (a, a)) ->
   Acc (Channel Z a)
distanceMap sh this others points =
   A.zipWith min
      (distanceMapContained sh this others)
      (distanceMapPoints (pixelCoordinates sh) points)

distanceMapRun ::
   DIM2 ->
   ((Float,Float),(Float,Float),(Int,Int)) ->
   [((Float,Float),(Float,Float),(Int,Int))] ->
   [Point2 Float] ->
   Channel Z Word8
distanceMapRun =
   let distances =
          Run.with CUDA.run1 $
          \sh this others points ->
             let scale =
                    case Exp.unlift (atom:.atom:.atom) sh of
                       _z:.y:.x -> (4/) $ A.fromIntegral $ min x y
             in  imageByteFromFloat $ A.map (scale*) $
                 distanceMap sh this others points
   in  \sh this others points ->
          distances sh this
             (A.fromList (Z :. length others) others)
             (A.fromList (Z :. length points) $ map (\(Point2 p) -> p) points)


emptyWeightedCanvas ::
   (A.Slice ix, A.Shape ix) =>
   ix :. Int :. Int ->
   (Channel Z Float, Channel ix Float)
emptyWeightedCanvas =
   Run.with CUDA.run1 $ \sh ->
      let (_ix :. height :. width) = unliftDim2 sh
      in  (A.fill (A.lift $ Z:.height:.width) 0,
           A.fill sh 0)


addToWeightedCanvas ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsNum a) =>
   (Acc (Channel Z a), Acc (Channel ix a)) ->
   (Acc (Channel Z a), Acc (Channel ix a)) ->
   (Acc (Channel Z a), Acc (Channel ix a))
addToWeightedCanvas (weight, pic) (weightSum, canvas) =
   (A.zipWith (+) weight weightSum,
    A.zipWith (+) canvas $ A.zipWith (*) pic $
    replicateChannel
       (A.indexTail $ A.indexTail $ A.shape pic)
       weight)

-- launch timeout
updateWeightedCanvasMerged ::
   ((Float,Float),(Float,Float),(Int,Int)) ->
   [((Float,Float),(Float,Float),(Int,Int))] ->
   [Point2 Float] ->
   Array DIM3 Word8 ->
   (Channel Z Float, Channel DIM1 Float) ->
   (Channel Z Float, Channel DIM1 Float)
updateWeightedCanvasMerged =
   let update =
          Run.with CUDA.run1 $
          \this others points pic (weightSum,canvas) ->
             let (rot, mov, _) =
                    Exp.unlift ((atom,atom), (atom,atom), atom) this
             in  addToWeightedCanvas
                    (distanceMap (A.shape weightSum) this others points,
                     snd $ rotateStretchMove rot mov (unliftDim2 $ A.shape canvas) $
                     separateChannels $ imageFloatFromByte pic)
                    (weightSum,canvas)
   in  \this others points pic canvas ->
          update this (A.fromList (Z :. length others) others)
             (A.fromList (Z :. length points) $ map (\(Point2 p) -> p) points)
             pic canvas

updateWeightedCanvas ::
   ((Float,Float),(Float,Float),(Int,Int)) ->
   [((Float,Float),(Float,Float),(Int,Int))] ->
   [Point2 Float] ->
   Array DIM3 Word8 ->
   (Channel Z Float, Channel DIM1 Float) ->
   (Channel Z Float, Channel DIM1 Float)
updateWeightedCanvas =
   let distances = Run.with CUDA.run1 distanceMap
       update =
          Run.with CUDA.run1 $
          \this pic dist (weightSum,canvas) ->
             let (rot, mov, _) =
                    Exp.unlift ((atom,atom), (atom,atom), atom) this
             in  addToWeightedCanvas
                    (dist,
                     snd $ rotateStretchMove rot mov (unliftDim2 $ A.shape canvas) $
                     separateChannels $ imageFloatFromByte pic)
                    (weightSum,canvas)
   in  \this others points pic (weightSum,canvas) ->
          update this
             pic
             (distances (A.arrayShape weightSum) this
                 (A.fromList (Z :. length others) others)
                 (A.fromList (Z :. length points) $ map (\(Point2 p) -> p) points))
             (weightSum,canvas)

-- launch timeout
updateWeightedCanvasSplit ::
   ((Float,Float),(Float,Float),(Int,Int)) ->
   [((Float,Float),(Float,Float),(Int,Int))] ->
   [Point2 Float] ->
   Array DIM3 Word8 ->
   (Channel Z Float, Channel DIM1 Float) ->
   (Channel Z Float, Channel DIM1 Float)
updateWeightedCanvasSplit =
   let update = Run.with CUDA.run1 addToWeightedCanvas
       distances = Run.with CUDA.run1 distanceMap
       rotated =
          Run.with CUDA.run1 $
          \sh rot mov pic ->
             snd $ rotateStretchMove rot mov (unliftDim2 sh) $
             separateChannels $ imageFloatFromByte pic
   in  \this@(rot, mov, _) others points pic (weightSum,canvas) ->
          update
             (distances (A.arrayShape weightSum) this
                 (A.fromList (Z :. length others) others)
                 (A.fromList (Z :. length points) $
                  map (\(Point2 p) -> p) points),
              rotated (A.arrayShape canvas) rot mov pic)
             (weightSum,canvas)


finalizeWeightedCanvas ::
   (Channel Z Float, Channel DIM1 Float) -> Array DIM3 Word8
finalizeWeightedCanvas =
   Run.with CUDA.run1 $
   \(weightSum, canvas) ->
      imageByteFromFloat $ interleaveChannels $
      A.zipWith (/) canvas $
      replicateChannel (A.indexTail $ A.indexTail $ A.shape canvas) weightSum


process :: Option.Args -> IO ()
process args = do
   let paths = Option.inputs args
   let opt = Option.option args
   putStrLn "\nfind rotation angles"
   picAngles <-
      forM paths $ \path -> do
         pic <- readImage path
         when False $ analyseRotations pic
         let angle = findOptimalRotation pic
         printf "%s %f\176\n" path angle
         return (path, (angle*pi/180, pic))

   putStrLn "\nfind relative placements"
   let rotated =
          map (mapSnd (prepareOverlapMatching (Option.smooth opt))) picAngles
   let pairs = do
          (a:as) <- tails $ zip [0..] rotated
          b <- as
          return (a,b)

   when True $ do
      putStrLn "write fft"
      let (_,pic0) : (_,pic1) : _ = rotated
          size = (Z:.512:.1024 :: DIM2)
      writeGrey 90 "/tmp/padded.jpeg" $
         CUDA.run1
            (imageByteFromFloat .
             pad 0 (A.lift size)) $
         pic0
      writeGrey 90 "/tmp/spectrum.jpeg" $
         CUDA.run $ imageByteFromFloat $ A.map Complex.real $
         FFT.fft2D FFT.Forward $
         CUDA.run1
            (A.map (A.lift . (:+ 0)) .
             pad 0 (A.lift size)) $
         pic0
      writeGrey 90 "/tmp/convolution.jpeg" $
         CUDA.run $ imageByteFromFloat $ A.map (0.000001*) $
         convolvePadded size (A.use pic0) (A.use pic1)

   let padSize = Option.padSize opt
   let (maybeAllOverlapsShared, optimalOverlapShared) =
          case Just $ Z :. padSize :. padSize of
             Just padExtent -> (Nothing, optimalOverlapBigFine padExtent)
             Nothing ->
                let (rotHeights, rotWidths) =
                       unzip $
                       map (\(_chans:.height:.width) -> (height, width)) $
                       map (A.arrayShape . snd) rotated
                    maxSum2 sizes =
                       case List.sortBy (flip compare) sizes of
                          size0 : size1 : _ -> size0+size1
                          _ -> error "less than one picture - there should be no pairs"
                    padWidth  = ceilingPow2 $ maxSum2 rotWidths
                    padHeight = ceilingPow2 $ maxSum2 rotHeights
                    padExtent = Z :. padHeight :. padWidth
                in  (Just $ allOverlapsRun padExtent,
                     optimalOverlap padExtent)

   displacements <-
      fmap catMaybes $
      forM pairs $ \((ia,(pathA,picA)), (ib,(pathB,picB))) -> do
         forM_ maybeAllOverlapsShared $ \allOverlapsShared -> when False $
            writeGrey 90
               (printf "/tmp/%s-%s-score.jpeg"
                  (FilePath.takeBaseName pathA) (FilePath.takeBaseName pathB)) $
               allOverlapsShared (Option.minimumOverlap opt) picA picB

         let d = fst $ optimalOverlapShared (Option.minimumOverlap opt) picA picB
         let diff = overlapDifferenceRun d picA picB
         let overlapping = diff < Option.maximumDifference opt
         printf "%s - %s, %s, difference %f%s\n" pathA pathB (show d) diff
            (if overlapping then "" else " unrelated -> ignoring")
         forM_ (Option.outputOverlap opt) $ \format ->
            writeImage 90
               (printf format
                  (FilePath.takeBaseName pathA) (FilePath.takeBaseName pathB)) $
               composeOverlap d (snd $ picAngles!!ia, snd $ picAngles!!ib)
         return $ toMaybe overlapping ((ia,ib), d)

   let (poss, dps) =
          absolutePositionsFromPairDisplacements (length rotated) displacements
   putStrLn "\nabsolute positions"
   mapM_ print poss

   putStrLn "\ncompare position differences with pair displacements"
   zipWithM_
      (\(dpx,dpy) (dx,dy) ->
         printf "(%f,%f) (%i,%i)\n" dpx dpy dx dy)
      dps (map snd displacements)
   let (errdx,errdy) =
          mapPair (maximum,maximum) $ unzip $
          zipWith
             (\(dpx,dpy) (dx,dy) ->
                (abs $ dpx - fromIntegral dx, abs $ dpy - fromIntegral dy))
             dps (map snd displacements)

   putStrLn ""
   printf "maximum horizontal error: %f\n" errdx
   printf "maximum vertical error: %f\n" errdy

   putStrLn "\ncompose all parts"
   let picRots =
          map (mapFst (\angle -> (cos angle, sin angle)) . snd) picAngles
       bboxes =
          map
             (\(rot, pic) ->
                case A.arrayShape pic of
                   Z:.height:.width:._chans ->
                      boundingBoxOfRotated rot
                         (fromIntegral width, fromIntegral height))
             picRots
       floatPoss =
          zipWith
             (\((l,_), (t,_)) (mx,my) -> (mx-l, my-t))
             bboxes
             (map (mapPair (realToFrac, realToFrac)) poss)
       ((canvasLeft,canvasRight), (canvasTop,canvasBottom)) =
          mapPair
             (mapPair (minimum, maximum) . unzip,
              mapPair (minimum, maximum) . unzip) $
          unzip $
          zipWith
             (\(mx,my) bbox ->
                mapPair (mapPair ((mx+), (mx+)), mapPair ((my+), (my+))) bbox)
             floatPoss bboxes
       canvasWidth  = ceiling (canvasRight-canvasLeft)
       canvasHeight = ceiling (canvasBottom-canvasTop)
       canvasShape = Z :. canvasHeight :. canvasWidth
   printf "canvas %f-%f, %f-%f\n" canvasLeft canvasRight canvasTop canvasBottom
   printf "canvas size %d, %d\n" canvasWidth canvasHeight
   forM_ (Option.outputHard opt) $ \path ->
      writeImage (Option.quality opt) path $
      finalizeCanvas $
      foldl
         (\canvas ((mx,my), (rot, pic)) ->
            updateCanvas rot (mx-canvasLeft, my-canvasTop) pic canvas)
         (emptyCanvas (Z :. 3 :. canvasHeight :. canvasWidth))
         (zip floatPoss picRots)

   putStrLn "\ndistance maps"
   let geometries =
          zipWith
             (\(mx,my) (rot, pic) ->
                let Z:.height:.width:._chans = A.arrayShape pic
                    mov = (mx-canvasLeft, my-canvasTop)
                    trans = Point2 . rotateStretchMovePoint rot mov
                    widthf  = fromIntegral width
                    heightf = fromIntegral height
                    corner00 = trans (0,0)
                    corner10 = trans (widthf,0)
                    corner01 = trans (0,heightf)
                    corner11 = trans (widthf,heightf)
                    corners = [corner00, corner01, corner10, corner11]
                    edges =
                       map (uncurry Line.Segment) $
                       [(corner00, corner10), (corner10, corner11),
                        (corner11, corner01), (corner01, corner00)]
                in  ((rot, mov, (width,height)), corners, edges))
             floatPoss picRots

   let geometryRelations =
          flip map (removeEach geometries) $
             \((thisGeom, thisCorners, thisEdges), others) ->
                let intPoints = intersections thisEdges $ concatMap thd3 others
                    overlappingCorners =
                       filter
                          (\(Point2 c) ->
                             any (\(rot, mov, (width,height)) ->
                                    inBoxPlain (width,height) $
                                    mapPair (round, round) $
                                    rotateStretchMoveBackPoint rot mov c) $
                             map fst3 others)
                          thisCorners
                    allPoints = intPoints ++ overlappingCorners
                    otherGeoms = map fst3 others
                in  (thisGeom, otherGeoms, allPoints)

   forM_ (zip geometryRelations picAngles) $
         \((thisGeom, otherGeoms, allPoints), (path, _)) -> do

      let stem = FilePath.takeBaseName path
      when True $ do
         writeGrey 90
            (printf "/tmp/%s-distance-box.jpeg" stem) $
            distanceMapBoxRun canvasShape thisGeom

         writeGrey 90
            (printf "/tmp/%s-distance-contained.jpeg" stem) $
            distanceMapContainedRun canvasShape thisGeom otherGeoms

         writeGrey 90
            (printf "/tmp/%s-distance-points.jpeg" stem) $
            distanceMapPointsRun canvasShape allPoints

      forM_ (Option.outputDistanceMap opt) $ \format ->
         writeGrey 90 (printf format stem) $
            distanceMapRun canvasShape thisGeom otherGeoms allPoints

   forM_ (Option.output opt) $ \path -> do
     putStrLn "\nweighted composition"
     writeImage (Option.quality opt) path $
      finalizeWeightedCanvas $
      foldl
         (\canvas ((thisGeom, otherGeoms, allPoints), (_rot, pic)) ->
            updateWeightedCanvas thisGeom otherGeoms allPoints pic canvas)
         (emptyWeightedCanvas (Z :. 3 :. canvasHeight :. canvasWidth))
         (zip geometryRelations picRots)

main :: IO ()
main = process =<< Option.get
