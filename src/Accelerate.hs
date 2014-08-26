{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.Array.Accelerate.Math.FFT as FFT
import qualified Data.Array.Accelerate.Math.Complex as Complex
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.IO as AIO
import qualified Data.Array.Accelerate.Arithmetic.LinearAlgebra as LinAlg
import qualified Data.Array.Accelerate.Utility.Lift.Acc as Acc
import qualified Data.Array.Accelerate.Utility.Arrange as Arrange
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Math.Complex (Complex, )
import Data.Array.Accelerate.Utility.Lift.Acc (acc, expr)
import Data.Array.Accelerate
          (Acc, Array, Exp, DIM1, DIM2, DIM3,
           (:.)((:.)), Z(Z), Any(Any), All(All),
           (<*), (<=*), (>=*), (==*), (&&*), (||*), (?), )

import qualified Data.Packed.Matrix as Matrix
import qualified Data.Packed.Vector as Vector
import qualified Data.Packed.ST as PackST
import Numeric.Container ((<\>), (<>))

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV

import qualified System.Environment as Env
import qualified System.FilePath as FilePath

import Text.Printf (printf)

import qualified Data.List.Key as Key
import qualified Data.List as List
import qualified Data.Bits as Bit
import Control.Monad.HT (void)
import Control.Monad (zipWithM_, when)
import Data.List.HT (mapAdjacent, tails)
import Data.Traversable (forM)
import Data.Foldable (foldMap)
import Data.Tuple.HT (mapSnd)
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
   A.lift1 $
       \(z :. chans :. height :. width) ->
          z :. height :. width :. chans :: ExpDIM3 Z

cycleRightDim3 :: Exp DIM3 -> Exp DIM3
cycleRightDim3 =
   A.lift1 $
       \(z :. height :. width :. chans) ->
          z :. chans :. height :. width :: ExpDIM3 Z

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
rotateStretchMoveCoords (rx,ry) (mx,my) (width,height) =
   let corr = recip $ rx*rx + ry*ry
       rot = (corr*rx, -corr*ry)
   in  A.generate (A.lift $ Z:.height:.width) $ \p ->
          let (_z :. ydst :. xdst) = unliftDim2 p
          in  A.lift $
              rotatePoint rot
                 (A.fromIntegral xdst - mx,
                  A.fromIntegral ydst - my)

validCoords ::
   (A.Elt a, A.IsFloating a) =>
   (Exp Int, Exp Int) ->
   Acc (Channel Z (a, a)) ->
   Acc (Channel Z Bool)
validCoords (width,height) =
   A.map $ A.lift1 $ \(x,y) ->
      let xi = fastRound x
          yi = fastRound y
      in  0<=*xi &&* xi<*width &&* 0<=*yi &&* yi<*height

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


unliftRotationParameters ::
   Acc ((A.Scalar Float, A.Scalar Float), Array DIM3 Word8) ->
   ((Exp Float, Exp Float), Acc (Array DIM3 Word8))
unliftRotationParameters = Acc.unlift ((expr,expr), acc)

rotateHistogram ::
   Float -> Array DIM3 Word8 -> (Array DIM3 Word8, Array DIM1 Float)
rotateHistogram =
   let rot =
          CUDA.run1 $ \arg ->
             let (orient, arr) = unliftRotationParameters arg
                 rotated =
                    rotate orient $
                    separateChannels $ imageFloatFromByte arr
             in  A.lift
                    (imageByteFromFloat $ interleaveChannels rotated,
                     rowHistogram rotated)
   in  \angle arr ->
          rot ((Acc.singleton $ cos angle, Acc.singleton $ sin angle), arr)


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
          CUDA.run1 $ \arg ->
             let (orient, arr) = unliftRotationParameters arg
             in  A.sum $ A.map (^(2::Int)) $ differentiate $ rowHistogram $
                 rotate orient $
                 separateChannels $ imageFloatFromByte arr
   in  \angle arr ->
          A.indexArray
             (rot ((Acc.singleton $ cos angle, Acc.singleton $ sin angle), arr)) Z

findOptimalRotation :: Array DIM3 Word8 -> Float
findOptimalRotation pic =
   Key.maximum (flip scoreRotation pic . (* (pi/180))) $
   map (\a -> fromIntegral a / 100) [-100,-95..100::Int]





rotateManifest :: Float -> Array DIM3 Word8 -> Array DIM3 Float
rotateManifest =
   let rot =
          CUDA.run1 $ \arg ->
             let (orient, arr) = unliftRotationParameters arg
             in  rotate orient $
                 separateChannels $ imageFloatFromByte arr
   in  \angle arr ->
          rot ((Acc.singleton $ cos angle, Acc.singleton $ sin angle), arr)

prepareOverlapMatching ::
   (Float, Array DIM3 Word8) -> Array DIM2 Float
prepareOverlapMatching =
   let rot =
          CUDA.run1 $ \arg ->
             let (orient, arr) = unliftRotationParameters arg
             in  rotate orient $
                 removeDCOffset $ brightnessPlane $
                 separateChannels $ imageFloatFromByte arr
   in  \(angle, arr) ->
          rot ((Acc.singleton $ cos angle, Acc.singleton $ sin angle), arr)


ceilingPow2Exp :: Exp Int -> Exp Int
ceilingPow2Exp n =
   A.setBit 0 $ A.ceiling $ logBase 2 (fromIntegral n :: Exp Double)

pad ::
   (A.Elt a, A.IsNum a) =>
   Exp DIM2 -> Acc (Array DIM2 a) -> Acc (Array DIM2 a)
pad sh arr =
   let (height, width) = A.unlift $ A.unindex2 $ A.shape arr
   in  A.generate sh $ \p ->
          let (y, x) = A.unlift $ A.unindex2 p
          in  (y<*height &&* x<*width)
              ?
              (arr A.! A.index2 y x, 0)

convolveImpossible ::
   (A.Elt a, A.IsFloating a) =>
   Acc (Array DIM2 a) -> Acc (Array DIM2 a) -> Acc (Array DIM2 a)
convolveImpossible x y =
   let (heightx, widthx) = A.unlift $ A.unindex2 $ A.shape x
       (heighty, widthy) = A.unlift $ A.unindex2 $ A.shape y
       width  = ceilingPow2Exp $ widthx  + widthy
       height = ceilingPow2Exp $ heightx + heighty
       sh = A.index2 height width
       forward z =
          FFT.fft2D FFT.Forward $ CUDA.run $
          A.map (A.lift . (Complex.:+ 0)) $ pad sh z
   in  A.map Complex.real $
       FFT.fft2D FFT.Inverse $ CUDA.run $
       A.zipWith (*) (forward x) (forward y)


ceilingPow2 :: Int -> Int
ceilingPow2 n =
   Bit.setBit 0 $ ceiling $ logBase 2 (fromIntegral n :: Double)

removeDCOffset ::
   (A.Elt a, A.IsFloating a) => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
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


convolvePadded ::
   (A.Elt a, A.IsFloating a) =>
   DIM2 -> Acc (Array DIM2 a) -> Acc (Array DIM2 a) -> Acc (Array DIM2 a)
convolvePadded sh@(Z :. height :. width) =
   let forward =
          FFT.fft2D' FFT.Forward width height .
          A.map (A.lift . (Complex.:+ 0)) . pad (A.lift sh)
       inverse = FFT.fft2D' FFT.Inverse width height
   in  \ x y ->
          A.map Complex.real $ inverse $
          A.zipWith (\xi yi -> xi * Complex.conj yi) (forward x) (forward y)


attachDisplacements ::
   (A.Elt a, A.IsScalar a) =>
   Exp Int -> Exp Int ->
   Acc (Array DIM2 a) -> Acc (Array DIM2 ((Int, Int), a))
attachDisplacements xsplit ysplit arr =
   let sh = A.shape arr
       (_z :. height :. width) = unliftDim2 sh
   in  A.generate sh $ \p ->
          let (_z:.y:.x) = unliftDim2 p
              wrap size split c = c<*split ? (c, c-size)
          in  A.lift ((wrap height ysplit y, wrap width xsplit x), arr A.! p)

weightOverlapScores ::
   (A.Elt a, A.IsFloating a, A.IsScalar a) =>
   Exp Int -> (Exp Int, Exp Int) -> (Exp Int, Exp Int) ->
   Acc (Array DIM2 ((Int, Int), a)) ->
   Acc (Array DIM2 ((Int, Int), a))
weightOverlapScores minOverlap (widtha,heighta) (widthb,heightb) =
   A.map
       (A.lift1 $ \(dp,v) ->
          let (dy,dx) = A.unlift dp
              clipWidth  = min widtha  (widthb  + dx) - max 0 dx
              clipHeight = min heighta (heightb + dy) - max 0 dy
          in  (dp :: Exp (Int, Int),
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
   Acc (Array DIM2 ((Int, Int), a)) ->
   Acc (Array DIM2 ((Int, Int), a))
minimumOverlapScores minOverlap (widtha,heighta) (widthb,heightb) =
   A.map
       (A.lift1 $ \(dp,v) ->
          let (dy,dx) = A.unlift dp
              clipWidth  = min widtha  (widthb  + dx) - max 0 dx
              clipHeight = min heighta (heightb + dy) - max 0 dy
          in  (dp :: Exp (Int, Int),
                 (clipWidth >=* minOverlap  &&*  clipHeight >=* minOverlap)
                 ?
                 (v, 0)))

argmax ::
   (A.Elt a, A.Elt b, A.IsScalar b) =>
   Exp (a, b) -> Exp (a, b) -> Exp (a, b)
argmax x y  =  A.snd x <* A.snd y ? (y,x)

argmaximum ::
   (A.Elt a, A.IsScalar a) =>
   Acc (Array DIM2 ((Int, Int), a)) -> Acc (A.Scalar ((Int, Int), a))
argmaximum = A.fold1All argmax


allOverlaps ::
   DIM2 ->
   Acc (Array DIM2 Float) -> Acc (Array DIM2 Float) ->
   Acc (Array DIM2 ((Int, Int), Float))
allOverlaps size@(Z :. height :. width) =
   let convolve = convolvePadded size
   in  \a b ->
          let (Z :. heighta :. widtha) = A.unlift $ A.shape a
              (Z :. heightb :. widthb) = A.unlift $ A.shape b
              half = flip div 2
              weight =
                 if False
                   then
                      weightOverlapScores 100
                         (widtha, heighta)
                         (widthb, heightb)
                   else
                      minimumOverlapScores 100
                         (widtha, heighta)
                         (widthb, heightb)
          in  weight $
              attachDisplacements
                 (half $ A.lift width - widthb + widtha)
                 (half $ A.lift height - heightb + heighta) $
              convolve a b


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


main :: IO ()
main = do
   paths <- Env.getArgs
   putStrLn "\nfind rotation angles"
   picAngles <-
      forM paths $ \path -> do
         pic <- readImage path
         when False $ analyseRotations pic
         let angle = findOptimalRotation pic
         printf "%s %f\176\n" path angle
         return (path, (angle*pi/180, pic))

   putStrLn "\nfind relative placements"
   let rotated = map (mapSnd prepareOverlapMatching) picAngles
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
             pad (A.lift size)) $
         pic0
      writeGrey 90 "/tmp/spectrum.jpeg" $
         CUDA.run $ imageByteFromFloat $ A.map Complex.real $
         FFT.fft2D FFT.Forward $
         CUDA.run1
            (A.map (A.lift . (Complex.:+ 0)) .
             pad (A.lift size)) $
         pic0
      writeGrey 90 "/tmp/convolution.jpeg" $
         CUDA.run $ imageByteFromFloat $ A.map (0.000001*) $
         convolvePadded size (A.use pic0) (A.use pic1)

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
       padSize = Z :. padHeight :. padWidth

   let composeOverlap =
          let rot (angle,pic) =
                 rotate (cos angle, sin angle) $
                 separateChannels $ imageFloatFromByte pic
              f =
                 CUDA.run1 $
                 Acc.modify ((expr,expr),((expr,acc),(expr,acc))) $
                 \((dx,dy), (a,b)) ->
                    imageByteFromFloat $ interleaveChannels $
                    overlap2 (dx, dy) (rot a, rot b)
          in  \(dx,dy) ((anglea,pica), (angleb,picb)) ->
                 f ((Acc.singleton dx, Acc.singleton dy),
                    ((Acc.singleton anglea, pica),
                     (Acc.singleton angleb, picb)))
   let allOverlapsRun =
          CUDA.run1 $ Acc.modify (acc,acc) $ \(picA, picB) ->
             imageByteFromFloat $
             A.map (0.0001*) $
             A.map A.snd $ allOverlaps padSize picA picB
   let optimalOverlap ::
          Array DIM2 Float -> Array DIM2 Float -> ((Int, Int), Float)
       optimalOverlap =
          let run =
                 CUDA.run1 $
                 argmaximum . A.uncurry (allOverlaps padSize)
          in  \a b -> A.indexArray (run (a,b)) Z

   displacements <-
      forM pairs $ \((ia,(pathA,picA)), (ib,(pathB,picB))) -> do
         when False $
            writeGrey 90
               (printf "/tmp/%s-%s-score.jpeg"
                  (FilePath.takeBaseName pathA) (FilePath.takeBaseName pathB)) $
               allOverlapsRun (picA, picB)

         let ((dy,dx), score) = optimalOverlap picA picB
         printf "%s - %s, %s %f\n" pathA pathB (show (dx,dy)) score
         writeImage 90
            (printf "/tmp/%s-%s.jpeg"
               (FilePath.takeBaseName pathA) (FilePath.takeBaseName pathB)) $
            composeOverlap (dx,dy) (snd $ picAngles!!ia, snd $ picAngles!!ib)
         return ((ia,ib), (dx,dy))

   let (poss, dps) =
          absolutePositionsFromPairDisplacements (length rotated) displacements
   putStrLn "\nabsolute positions"
   mapM_ print poss

   putStrLn "\ncompare position differences with pair displacements"
   zipWithM_
      (\(dpx,dpy) (dx,dy) ->
         printf "(%f,%f) (%i,%i)\n" dpx dpy dx dy)
      dps (map snd displacements)
