{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Option

import qualified Data.Array.Accelerate.Fourier.Real as FourierReal
import qualified Data.Array.Accelerate.CUFFT.Single as CUFFT
import qualified Data.Array.Accelerate.Data.Complex as Complex
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.IO as AIO
import qualified Data.Array.Accelerate.Arithmetic.LinearAlgebra as LinAlg
import qualified Data.Array.Accelerate.Utility.Lift.Run as Run
import qualified Data.Array.Accelerate.Utility.Lift.Acc as Acc
import qualified Data.Array.Accelerate.Utility.Lift.Exp as Exp
import qualified Data.Array.Accelerate.Utility.Arrange as Arrange
import qualified Data.Array.Accelerate.Utility.Loop as Loop
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Complex (Complex((:+)), )
import Data.Array.Accelerate.Utility.Lift.Exp (expr)
import Data.Array.Accelerate.Utility.Ord (argmaximum)
import Data.Array.Accelerate
          (Acc, Array, Exp, DIM1, DIM2, DIM3,
           (:.)((:.)), Z(Z), Any(Any), All(All),
           (<*), (<=*), (>=*), (==*), (&&*), (||*), (?), )

import qualified Data.Packed.Matrix as Matrix
import qualified Data.Packed.Vector as Vector
import qualified Data.Packed.ST as PackST
import qualified Numeric.Container as Container
import Numeric.Container ((<\>), (<>))

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Data.Complex as HComplex

import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV

import qualified System.FilePath as FilePath

import qualified Distribution.Simple.Utils as CmdLine
import Distribution.Verbosity (Verbosity)
import Text.Printf (printf)

import qualified Data.List.Key as Key
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Bits as Bit
import Control.Monad.HT (void)
import Control.Monad (liftM2, zipWithM_, when, guard)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (catMaybes)
import Data.List.HT (removeEach, mapAdjacent, tails)
import Data.Traversable (forM)
import Data.Foldable (forM_, foldMap)
import Data.Tuple.HT (mapPair, mapFst, mapSnd, fst3, thd3)
import Data.Word (Word8)

import System.IO.Unsafe (unsafePerformIO)


readImage :: Verbosity -> FilePath -> IO (Array DIM3 Word8)
readImage verbosity path = do
   epic <- Pic.readImage path
   case epic of
      Left msg -> ioError $ userError msg
      Right dynpic ->
         case dynpic of
            Pic.ImageYCbCr8 pic -> do
               let dat = Pic.imageData pic
               CmdLine.info verbosity $
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
   Exp.modify (expr :. expr :. expr :. expr) $
       \(z :. chans :. height :. width) ->
          z :. height :. width :. chans

cycleRightDim3 :: Exp DIM3 -> Exp DIM3
cycleRightDim3 =
   Exp.modify (expr :. expr :. expr :. expr) $
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


rotateLeftTop ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   (Exp a, Exp a) -> Acc (Channel ix a) ->
   ((Acc (A.Scalar a), Acc (A.Scalar a)), Acc (Channel ix a))
rotateLeftTop rot arr =
   let (chans :. height :. width) = unliftDim2 $ A.shape arr
       ((left, right), (top, bottom)) =
          boundingBoxOfRotated rot (A.fromIntegral width, A.fromIntegral height)
   in  ((A.unit left, A.unit top),
        snd $
        rotateStretchMove rot (-left,-top)
           (chans :. A.ceiling (bottom-top) :. A.ceiling (right-left)) arr)

rotate ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   (Exp a, Exp a) ->
   Acc (Channel ix a) -> Acc (Channel ix a)
rotate rot arr = snd $ rotateLeftTop rot arr


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


{-
duplicate of Graphics.Gnuplot.Utility.linearScale
-}
linearScale :: Fractional a => Int -> (a,a) -> [a]
linearScale n (x0,x1) =
   map (\m -> x0 + (x1-x0) * fromIntegral m / fromIntegral n) [0..n]

analyseRotations :: [Float] -> Array DIM3 Word8 -> IO ()
analyseRotations angles pic = do
   histograms <-
      forM angles $ \degree -> do
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
   in  \angle arr -> Acc.the $ rot (cos angle, sin angle) arr

findOptimalRotation :: [Float] -> Array DIM3 Word8 -> Float
findOptimalRotation angles pic =
   Key.maximum (flip scoreRotation pic . (* (pi/180))) angles


magnitudeSqr :: (A.Elt a, A.IsNum a) => Exp (Complex a) -> Exp a
magnitudeSqr =
   Exp.modify (expr:+expr) $ \(r:+i) -> r*r+i*i

complexFromReal :: (A.Elt a, A.IsNum a) => Exp a -> Exp (Complex a)
complexFromReal = A.lift . (:+0)

fourierTransformationRun :: Array DIM3 Word8 -> IO (Array DIM2 Word8)
fourierTransformationRun pic = do
   let (shape@(Z:.height:.width):._) = A.arrayShape pic
   plan <- CUFFT.plan2D CUFFT.forwardComplex shape
   let trans =
          Run.with CUDA.run1 $ \arr ->
             imageByteFromFloat $
             A.map (1e-9*) $
             A.zipWith (*)
                (A.map
                    (Exp.modify (expr,expr) $ \(k,j) ->
                       magnitudeSqr $ A.lift $
                       A.fromIntegral k :+ A.fromIntegral j) $
                 displacementMap
                    (A.constant (div width 2))
                    (A.constant (div height 2))
                    (A.constant shape)) $
             A.map magnitudeSqr $
             CUFFT.transform plan $
             A.map complexFromReal $
             brightnessPlane $ separateChannels $
             imageFloatFromByte arr
   return $ trans pic

fourierTransformation :: Option.Option -> FilePath -> Array DIM3 Word8 -> IO ()
fourierTransformation opt path pic = do
   let stem = FilePath.takeBaseName path
   spec <- fourierTransformationRun pic
   writeGrey (Option.quality opt)
      (printf "/tmp/%s-spectrum.jpeg" stem) spec

{-
Spectrum coefficients with high indices
are disturbed by periodization artifacts.
That is, we might fade out the picture at the borders
or soften the disturbed spectrum coefficients.
However, I do not know for a universally good solution,
thus I leave it as it is.
-}
scoreSlopes ::
   (A.Elt a, A.IsFloating a) =>
   (Exp Int, Exp Int) ->
   Acc (Channel Z (Complex a)) -> Acc (Array DIM1 a)
scoreSlopes (minX, maxX) arr =
   let shape = A.shape arr
       (_z:.height:.width) = Exp.unlift (expr:.expr:.expr) shape
       width2 = div width 2
       height2 = div height 2
       weighted =
          A.zipWith (*) (A.map magnitudeSqr arr) $
          A.map
             (Exp.modify (expr,expr) $ \(k,j) ->
                magnitudeSqr $ A.lift $
                A.fromIntegral k :+ A.fromIntegral j) $
          displacementMap width2 height2 shape
   in  A.fold (+) 0 $
       A.generate (A.lift (Z:.maxX-minX+1:.height2)) $
       Exp.modify (expr:.expr:.expr) $ \(_z:.x:.y) ->
          let (xi,frac) =
                 splitFraction $
                 A.fromIntegral (x + minX) *
                    A.fromIntegral y / A.fromIntegral height2
              z0 = weighted A.! A.lift (Z :. y :. mod xi width)
              z1 = weighted A.! A.lift (Z :. y :. mod (xi+1) width)
          in  linearIp (z0,z1) frac

radonAngle :: (Float, Float) -> Array DIM3 Word8 -> IO Float
radonAngle (minAngle,maxAngle) pic = do
   let (shape@(Z :. height :. _width):._) = A.arrayShape pic
   plan <- CUFFT.plan2D CUFFT.forwardComplex shape
   let height2 = fromIntegral (div height 2)
   let slope w = tan (w*pi/180) * height2
   let minX = floor $ slope minAngle
   let maxX = ceiling $ slope maxAngle
   let angle s = atan (s/height2) * 180/pi
   let trans =
          Run.with CUDA.run1 $ \arr ->
             A.map A.snd $ argmaximum $
             Arrange.mapWithIndex (\ix s -> A.lift (s, A.unindex1 ix)) $
             scoreSlopes (A.constant minX, A.constant maxX) $
             CUFFT.transform plan $
             A.map complexFromReal $
             brightnessPlane $ separateChannels $
             imageFloatFromByte arr
   return $ angle $ fromIntegral $ Acc.the (trans pic) + minX


rotateManifest :: Float -> Array DIM3 Word8 -> Array DIM3 Float
rotateManifest =
   let rot =
          Run.with CUDA.run1 $ \orient arr ->
             rotate orient $ separateChannels $ imageFloatFromByte arr
   in  \angle arr -> rot (cos angle, sin angle) arr


prepareOverlapMatching ::
   Int -> (Float, Array DIM3 Word8) -> ((Float,Float), Channel Z Float)
prepareOverlapMatching =
   let rot =
          Run.with CUDA.run1 $ \radius orient arr ->
             rotateLeftTop orient $
             (if True
                then highpass radius
                else removeDCOffset) $
             brightnessPlane $ separateChannels $ imageFloatFromByte arr
   in  \radius (angle, arr) ->
          mapFst (mapPair (Acc.the, Acc.the)) $
          rot radius (cos angle, sin angle) arr


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

mulConj ::
   (A.Elt a, A.IsFloating a) =>
   Exp (Complex a) -> Exp (Complex a) -> Exp (Complex a)
mulConj x y = x * Complex.conjugate y


fft2DGen ::
   (A.Elt e, CUFFT.Real e) =>
   CUFFT.Mode DIM2 e a b -> DIM2 -> CUFFT.Transform DIM2 a b
fft2DGen mode sh =
   CUFFT.transform $ unsafePerformIO $ CUFFT.plan2D mode sh

fft2DPlain ::
   (A.Elt a, CUFFT.Real a) =>
   CUFFT.Mode DIM2 a (Complex a) (Complex a) ->
   Channel Z (Complex a) -> Acc (Channel Z (Complex a))
fft2DPlain mode arr =
   A.use $ CUDA.run1 (fft2DGen mode $ A.arrayShape arr) arr

fft2D ::
   (A.Elt a, CUFFT.Real a) =>
   CUFFT.Mode DIM2 a (Complex a) (Complex a) ->
   Int -> Int ->
   Acc (Channel Z (Complex a)) -> Acc (Channel Z (Complex a))
fft2D mode width height = fft2DGen mode (Z:.height:.width)


correlateImpossible ::
   (A.Elt a, CUFFT.Real a) =>
   Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
correlateImpossible x y =
   let (heightx, widthx) = A.unlift $ A.unindex2 $ A.shape x
       (heighty, widthy) = A.unlift $ A.unindex2 $ A.shape y
       width  = ceilingPow2Exp $ widthx  + widthy
       height = ceilingPow2Exp $ heightx + heighty
       sh = A.index2 height width
       forward z =
          fft2DPlain CUFFT.forwardComplex $ CUDA.run $
          A.map complexFromReal $ pad 0 sh z
   in  A.map Complex.real $
       fft2DPlain CUFFT.inverseComplex $ CUDA.run $
       A.zipWith mulConj (forward x) (forward y)


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


correlatePaddedSimple ::
   (A.Elt a, CUFFT.Real a) =>
   DIM2 -> Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
correlatePaddedSimple sh@(Z :. height :. width) =
   let forward =
          fft2D CUFFT.forwardComplex width height .
          A.map complexFromReal . pad 0 (A.lift sh)
       inverse = fft2D CUFFT.inverseComplex width height
   in  \ x y ->
          A.map Complex.real $ inverse $
          A.zipWith mulConj (forward x) (forward y)


{-
This is more efficient than 'correlatePaddedSimple'
since it needs only one forward Fourier transform,
where 'correlatePaddedSimple' needs two of them.
For the analysis part,
perform two real-valued Fourier transforms using one complex-valued transform.
Afterwards we untangle the superposed spectra.
-}
correlatePadded ::
   (A.Elt a, CUFFT.Real a) =>
   DIM2 -> Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
correlatePadded sh@(Z :. height :. width) =
   let forward = fft2D CUFFT.forwardComplex width height
       inverse = fft2D CUFFT.inverseComplex width height
   in  \ a b ->
          A.map Complex.real $ inverse $
          A.map (A.uncurry mulConj) $
          FourierReal.untangleSpectra2d $ forward $
          pad 0 (A.lift sh) $
          A.zipWith (Exp.modify2 expr expr (:+)) a b


wrap :: Exp Int -> Exp Int -> Exp Int -> Exp Int
wrap size split c = c<*split ? (c, c-size)

displacementMap ::
   Exp Int -> Exp Int -> Exp DIM2 -> Acc (Channel Z (Int, Int))
displacementMap xsplit ysplit sh =
   let (_z :. height :. width) = unliftDim2 sh
   in  A.generate sh $ \p ->
          let (_z:.y:.x) = unliftDim2 p
          in  A.lift (wrap width xsplit x, wrap height ysplit y)

attachDisplacements ::
   (A.Elt a, A.IsScalar a) =>
   Exp Int -> Exp Int ->
   Acc (Channel Z a) -> Acc (Channel Z (a, (Int, Int)))
attachDisplacements xsplit ysplit arr =
   A.zip arr $ displacementMap xsplit ysplit (A.shape arr)

weightOverlapScores ::
   (A.Elt a, A.IsFloating a, A.IsScalar a) =>
   Exp Int -> (Exp Int, Exp Int) -> (Exp Int, Exp Int) ->
   Acc (Channel Z (a, (Int, Int))) ->
   Acc (Channel Z (a, (Int, Int)))
weightOverlapScores minOverlap (widtha,heighta) (widthb,heightb) =
   A.map
       (Exp.modify (expr,(expr,expr)) $ \(v, dp@(dy,dx)) ->
          let clipWidth  = min widtha  (widthb  + dx) - max 0 dx
              clipHeight = min heighta (heightb + dy) - max 0 dy
          in  ((clipWidth >=* minOverlap  &&*  clipHeight >=* minOverlap)
               ?
               (v / (A.fromIntegral clipWidth * A.fromIntegral clipHeight), 0),
               dp))

{- |
Set all scores to zero within a certain border.
Otherwise the matching algorithm will try to match strong bars at the borders
that are actually digitalization artifacts.
-}
minimumOverlapScores ::
   (A.Elt a, A.IsFloating a, A.IsScalar a) =>
   Exp Int -> (Exp Int, Exp Int) -> (Exp Int, Exp Int) ->
   Acc (Channel Z (a, (Int, Int))) ->
   Acc (Channel Z (a, (Int, Int)))
minimumOverlapScores minOverlap (widtha,heighta) (widthb,heightb) =
   A.map
       (Exp.modify (expr,(expr,expr)) $ \(v, dp@(dy,dx)) ->
          let clipWidth  = min widtha  (widthb  + dx) - max 0 dx
              clipHeight = min heighta (heightb + dy) - max 0 dy
          in  ((clipWidth >=* minOverlap  &&*  clipHeight >=* minOverlap)
               ?
               (v, 0),
               dp))


allOverlaps ::
   DIM2 ->
   Exp Float ->
   Acc (Channel Z Float) -> Acc (Channel Z Float) ->
   Acc (Channel Z (Float, (Int, Int)))
allOverlaps size@(Z :. height :. width) minOverlapPortion =
   let correlate = correlatePadded size
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
              correlate a b


allOverlapsRun ::
   DIM2 -> Float -> Channel Z Float -> Channel Z Float -> Channel Z Word8
allOverlapsRun padExtent =
   Run.with CUDA.run1 $ \minOverlap picA picB ->
      imageByteFromFloat $
      -- A.map (2*) $
      A.map (0.0001*) $
      A.map A.fst $ allOverlaps padExtent minOverlap picA picB

optimalOverlap ::
   DIM2 -> Float -> Channel Z Float -> Channel Z Float -> (Float, (Int, Int))
optimalOverlap padExtent =
   let run =
          Run.with CUDA.run1 $ \minimumOverlap a b ->
          argmaximum $ allOverlaps padExtent minimumOverlap a b
   in  \overlap a b -> Acc.the $ run overlap a b


shrink ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   GenDIM2 (Exp Int) -> Acc (Channel ix a) -> Acc (Channel ix a)
shrink (_:.yk:.xk) arr =
   let (shape:.height:.width) = unliftDim2 $ A.shape arr
   in  A.map (/ (A.fromIntegral xk * A.fromIntegral yk)) $
       A.fold1 (+) $ A.fold1 (+) $
       A.backpermute
          (A.lift $ shape :. div height yk :. div width xk :. yk :. xk)
          (Exp.modify (expr:.expr:.expr:.expr:.expr) $
           \(z:.yi:.xi:.yj:.xj) -> z:.yi*yk+yj:.xi*xk+xj)
          arr

-- cf. numeric-prelude
divUp :: (Integral a) => a -> a -> a
divUp a b = - div (-a) b


type GenDIM2 a = Z :. a :. a

shrinkFactors :: (Integral a) => DIM2 -> GenDIM2 a -> GenDIM2 a -> GenDIM2 a
shrinkFactors (Z:.heightPad:.widthPad)
   (Z :. heighta :. widtha) (Z :. heightb :. widthb) =
      let yk = divUp (heighta+heightb) $ fromIntegral heightPad
          xk = divUp (widtha +widthb)  $ fromIntegral widthPad
      in  Z :. yk :. xk

{-
Reduce image sizes below the padExtent before matching images.
-}
optimalOverlapBig ::
   DIM2 -> Float -> Channel Z Float -> Channel Z Float -> (Float, (Int, Int))
optimalOverlapBig padExtent =
   let run =
          Run.with CUDA.run1 $ \minimumOverlap a b ->
             let factors@(_z:.yk:.xk) =
                    shrinkFactors padExtent
                       (A.unlift $ A.shape a) (A.unlift $ A.shape b)
                 scalePos =
                    Exp.modify (expr, (expr,expr)) $
                    \(score, (xm,ym)) -> (score, (xm*xk, ym*yk))
             in  A.map scalePos $ argmaximum $
                 allOverlaps padExtent minimumOverlap
                    (shrink factors a) (shrink factors b)
   in  \minimumOverlap a b -> Acc.the $ run minimumOverlap a b


clip ::
   (A.Slice ix, A.Shape ix, A.Elt a) =>
   (Exp Int, Exp Int) ->
   (Exp Int, Exp Int) ->
   Acc (Channel ix a) -> Acc (Channel ix a)
clip (left,top) (width,height) arr =
   A.backpermute
      (A.lift $ A.indexTail (A.indexTail (A.shape arr)) :. height :. width)
      (Exp.modify (expr:.expr:.expr) $
       \(z :. y :. x) -> z :. y+top :. x+left)
      arr


overlappingArea ::
   (Ord a, Num a) =>
   GenDIM2 a ->
   GenDIM2 a ->
   (a, a) -> ((a, a), (a, a), (a, a))
overlappingArea (Z :. heighta :. widtha) (Z :. heightb :. widthb) (dx, dy) =
   let left = max 0 dx
       top  = max 0 dy
       right  = min widtha  (widthb  + dx)
       bottom = min heighta (heightb + dy)
       width  = right - left
       height = bottom - top
   in  ((left, top), (right, bottom), (width, height))


{-
Like 'optimalOverlapBig'
but computes precise distance in a second step
using a part in the overlapping area.
-}
optimalOverlapBigFine ::
   DIM2 -> Float -> Channel Z Float -> Channel Z Float -> (Float, (Int, Int))
optimalOverlapBigFine padExtent@(Z:.heightPad:.widthPad) =
   let overlaps = allOverlaps padExtent
       run =
          Run.with CUDA.run1 $ \minimumOverlap a b ->
             let shapeA = A.unlift $ A.shape a
                 shapeB = A.unlift $ A.shape b
                 factors@(_z:.yk:.xk) = shrinkFactors padExtent shapeA shapeB
                 coarsed@(coarsedx,coarsedy) =
                    mapPair ((xk*), (yk*)) $
                    Exp.unliftPair $ A.snd $ A.the $ argmaximum $
                    overlaps minimumOverlap
                       (shrink factors a) (shrink factors b)

                 ((leftOverlap, topOverlap), _,
                  (widthOverlap, heightOverlap))
                    = overlappingArea shapeA shapeB coarsed

                 widthFocus  = min widthOverlap $ A.lift $ div widthPad 2
                 heightFocus = min heightOverlap $ A.lift $ div heightPad 2
                 extentFocus = (widthFocus,heightFocus)
                 leftFocus = leftOverlap + div (widthOverlap-widthFocus) 2
                 topFocus  = topOverlap  + div (heightOverlap-heightFocus) 2
                 addCoarsePos =
                    Exp.modify (expr, (expr,expr)) $
                    \(score, (xm,ym)) -> (score, (xm+coarsedx, ym+coarsedy))
             in  A.map addCoarsePos $ argmaximum $
                 overlaps minimumOverlap
                    (clip (leftFocus,topFocus) extentFocus a)
                    (clip (leftFocus-coarsedx,topFocus-coarsedy) extentFocus b)
   in  \minimumOverlap a b -> Acc.the $ run minimumOverlap a b


{-
Like 'optimalOverlapBigFine'
but computes precise distances between many point pairs in a second step
using many parts in the overlapping area.
These point correspondences
can be used to compute corrections to rotation angles.
-}
optimalOverlapBigMulti ::
   DIM2 -> DIM2 -> Int ->
   Float -> Float -> Channel Z Float -> Channel Z Float ->
   [(Float, (Int, Int), (Int, Int))]
optimalOverlapBigMulti padExtent (Z:.heightStamp:.widthStamp) numCorrs =
   let overlapShrunk =
          Run.with CUDA.run1 $
          \minimumOverlap factors a b ->
             argmaximum $
             allOverlaps padExtent minimumOverlap
                (shrink factors a) (shrink factors b)
       diffShrunk =
          Run.with CUDA.run1 $
          \shrunkd factors a b ->
             overlapDifference shrunkd
                (shrink factors a) (shrink factors b)

       allOverlapsFine = allOverlaps (Z :. 2*heightStamp :. 2*widthStamp)
       overlapFine =
          Run.with CUDA.run1 $
          \minimumOverlap a b anchorA@(leftA, topA) anchorB@(leftB, topB)
                extent@(width,height) ->
             let addCoarsePos =
                    Exp.modify (expr, (expr,expr)) $
                    \(score, (xm,ym)) ->
                       let xc = div (width+xm) 2
                           yc = div (height+ym) 2
                       in  (score,
                            (leftA+xc,    topA+yc),
                            (leftB+xc-xm, topB+yc-ym))
             in  A.map addCoarsePos $ argmaximum $
                 allOverlapsFine minimumOverlap
                    (clip anchorA extent a)
                    (clip anchorB extent b)

   in  \maximumDiff minimumOverlap a b ->
          let factors@(Z:.yk:.xk) =
                 shrinkFactors padExtent (A.arrayShape a) (A.arrayShape b)

              (_score, (shrunkdx, shrunkdy)) =
                 Acc.the $ overlapShrunk minimumOverlap factors a b

              coarsedx = shrunkdx * xk
              coarsedy = shrunkdy * yk
              coarsed = (coarsedx,coarsedy)

              diff = Acc.the $ diffShrunk (shrunkdx, shrunkdy) factors a b

              ((leftOverlap, topOverlap),
               (rightOverlap, bottomOverlap),
               (widthOverlap, heightOverlap))
                 = overlappingArea (A.arrayShape a) (A.arrayShape b) coarsed

              widthStampClip = min widthOverlap widthStamp
              heightStampClip = min heightOverlap heightStamp

          in  (if diff < maximumDiff then id else const []) $
              map
                 (\(x,y) ->
                    Acc.the $
                    overlapFine minimumOverlap a b
                       (x, y) (x-coarsedx, y-coarsedy)
                       (widthStampClip, heightStampClip)) $
              zip
                 (map round $ tail $ init $
                  linearScale (numCorrs+1)
                     (fromIntegral leftOverlap :: Double,
                      fromIntegral $ rightOverlap - widthStampClip))
                 (map round $ tail $ init $
                  linearScale (numCorrs+1)
                     (fromIntegral topOverlap :: Double,
                      fromIntegral $ bottomOverlap - heightStampClip))


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
   in  \d a b -> Acc.the $ diff d a b


-- we cannot use leastSquaresSelected here, because the right-hand side is not zero
absolutePositionsFromPairDisplacements ::
   Int -> [((Int, Int), (Float, Float))] ->
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
       pxs = matrix <\> Vector.fromList (map realToFrac dxs)
       pys = matrix <\> Vector.fromList (map realToFrac dys)
   in  (zip (0 : Vector.toList pxs) (0 : Vector.toList pys),
        zip (Vector.toList $ matrix <> pxs) (Vector.toList $ matrix <> pys))


leastSquaresSelected ::
   Matrix.Matrix Double -> [Maybe Double] ->
   ([Double], [Double])
leastSquaresSelected m mas =
   let (lhsCols,rhsCols) =
          ListHT.unzipEithers $
          zipWith
             (\col ma ->
                case ma of
                   Nothing -> Left col
                   Just a -> Right $ Container.scale a col)
             (Matrix.toColumns m) mas
       lhs = Matrix.fromColumns lhsCols
       rhs = foldl1 Container.add rhsCols
       sol = lhs <\> Container.scale (-1) rhs
   in  (snd $
        List.mapAccumL
           (curry $ \x ->
               case x of
                  (as, Just a) -> (as, a)
                  (a:as, Nothing) -> (as, a)
                  ([], Nothing) -> error "too few elements in solution vector")
           (Vector.toList sol) mas,
        Vector.toList $
        Container.add (lhs <> sol) rhs)

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
   Int -> [((Int, (Float, Float)), (Int, (Float, Float)))] ->
   ([((Double,Double), HComplex.Complex Double)],
    [(Double,Double)])
layoutFromPairDisplacements numPics correspondences =
   let {-
       The weight will only influence the result
       for under-constrained equation systems.
       This is usually not the case.
       -}
       weight =
          let xs =
                 concatMap
                    (\((_ia,(xai,yai)),(_ib,(xbi,ybi))) -> [xai, yai, xbi, ybi])
                    correspondences
          in  realToFrac $ maximum xs - minimum xs
       matrix = PackST.runSTMatrix $ do
          mat <- PackST.newMatrix 0 (2 * length correspondences) (4*numPics)
          zipWithM_
             (\k ((ia,(xai,yai)),(ib,(xbi,ybi))) -> do
                let xa = realToFrac xai
                let xb = realToFrac xbi
                let ya = realToFrac yai
                let yb = realToFrac ybi
                PackST.writeMatrix mat (k+0) (4*ia+0) (-weight)
                PackST.writeMatrix mat (k+1) (4*ia+1) (-weight)
                PackST.writeMatrix mat (k+0) (4*ia+2) (-xa)
                PackST.writeMatrix mat (k+0) (4*ia+3) ya
                PackST.writeMatrix mat (k+1) (4*ia+2) (-ya)
                PackST.writeMatrix mat (k+1) (4*ia+3) (-xa)
                PackST.writeMatrix mat (k+0) (4*ib+0) weight
                PackST.writeMatrix mat (k+1) (4*ib+1) weight
                PackST.writeMatrix mat (k+0) (4*ib+2) xb
                PackST.writeMatrix mat (k+0) (4*ib+3) (-yb)
                PackST.writeMatrix mat (k+1) (4*ib+2) yb
                PackST.writeMatrix mat (k+1) (4*ib+3) xb)
             [0,2..] correspondences
          return mat
       {-
       We fix the first image to position (0,0) and rotation (1,0)
       in order to make the solution unique.
       -}
       (solution, projection) =
          leastSquaresSelected matrix
             (take (4*numPics) $
              map Just [0,0,1,0] ++ repeat Nothing)
   in  (map (\[dx,dy,rx,ry] -> ((weight*dx,weight*dy), rx HComplex.:+ ry)) $
        ListHT.sliceVertical 4 solution,
        map (\[x,y] -> (x,y)) $
        ListHT.sliceVertical 2 projection)


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



type Line2 a = (Point2 a, Point2 a)

intersect ::
   (Ord a, Fractional a) => Line2 a -> Line2 a -> Maybe (Point2 a)
intersect ((xa,ya), (xb,yb)) ((xc,yc), (xd,yd)) = do
   let denom = (xb-xa)*(yd-yc)-(xd-xc)*(yb-ya)
       r     = ((xd-xc)*(ya-yc)-(xa-xc)*(yd-yc)) / denom
       s     = ((xb-xa)*(ya-yc)-(xa-xc)*(yb-ya)) / denom
   guard (denom/=0)
   guard (0<=r && r<=1)
   guard (0<=s && s<=1)
   return (xa + r*(xb-xa), ya + r*(yb-ya))

intersections ::
   (Fractional a, Ord a) =>
   [Line2 a] -> [Line2 a] -> [Point2 a]
intersections segments0 segments1 =
   catMaybes $ liftM2 intersect segments0 segments1


type Point2 a = (a,a)

projectPerp ::
   (Fractional a) =>
   Point2 a -> (Point2 a, Point2 a) -> (a, Point2 a)
projectPerp (xc,yc) ((xa,ya), (xb,yb)) =
   let dx = xb-xa
       dy = yb-ya
       r = ((xc-xa)*dx + (yc-ya)*dy) / (dx*dx + dy*dy)
   in  (r, (xa + r*dx, ya + r*dy))

project ::
   (A.Elt a, A.IsFloating a) =>
   Point2 (Exp a) ->
   (Point2 (Exp a), Point2 (Exp a)) ->
   (Exp Bool, Point2 (Exp a))
project x ab =
   let (r, y) = projectPerp x ab
   in  (0<=*r &&* r<=*1, y)


distance :: (Floating a) => Point2 a -> Point2 a -> a
distance (xa,ya) (xb,yb) =
   sqrt $ (xa-xb)^(2::Int) + (ya-yb)^(2::Int)


distanceMapEdges ::
   (A.Elt a, A.IsFloating a) =>
   Exp DIM2 -> Acc (Array DIM1 ((a,a),(a,a))) -> Acc (Channel Z a)
distanceMapEdges sh edges =
   A.map (Exp.modify (expr,expr) $ \(valid, dist) -> valid ? (dist, 0)) $
   maskedMinimum $
   outerVector
      (Exp.modify2 (expr,expr) ((expr, expr), (expr, expr)) $ \p (q0, q1) ->
         mapSnd (distance p) $ project p (q0, q1))
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
          Exp.unlift ((expr,expr),(expr,expr),(expr,expr)) geom
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
   (A.Slice ix, A.Shape ix, A.Elt a, A.Elt b, A.Elt c) =>
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
      (Exp.modify2 (expr, ((expr, expr), (expr, expr))) (expr,expr) $
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
          A.map (Exp.modify (expr,expr) $
                   \(valid, dist) -> valid ? (scale*dist, 0)) $
          maskedMinimum $
          A.map (Exp.mapSnd A.fst) $
          separateDistanceMap $
          distanceMapBox sh geom


-- maybe move to Accelerate.Utility
{- |
We use it as a work-around.
Fusion of 'fold1' and 'replicate' would be very welcome
but it seems to fail with current accelerate version.
-}
breakFusion :: (A.Arrays a) => Acc a -> Acc a
breakFusion = id A.>-> id

array1FromList :: (A.Elt a) => [a] -> Array DIM1 a
array1FromList xs = A.fromList (Z :. length xs) xs


containedAnywhere ::
   (A.Elt a, A.IsFloating a) =>
   Acc (Array DIM1 ((a,a), (a,a), (Int,Int))) ->
   Acc (Array DIM3 (a,a)) ->
   Acc (Array DIM3 Bool)
containedAnywhere geoms arr =
   A.fold1 (||*) $
   breakFusion $
   outerVector
      (Exp.modify2 (expr,expr) ((expr,expr),(expr,expr),(expr,expr)) $
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
   in  A.map (Exp.modify (expr,expr) $
                \(valid, dist) -> valid ? (dist, 0)) $
       maskedMinimum $
       A.zipWith
          (Exp.modify2 expr (expr,(expr,expr)) $ \c (b,(dist,_)) ->
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
          distances sh this $ array1FromList others


pixelCoordinates ::
   (A.Elt a, A.IsFloating a) =>
   Exp DIM2 -> Acc (Channel Z (a,a))
pixelCoordinates sh =
   A.generate sh $ Exp.modify (expr:.expr:.expr) $ \(_z:.y:.x) ->
      (A.fromIntegral x, A.fromIntegral y)

distanceMapPoints ::
   (A.Slice ix, A.Shape ix, A.Elt a, A.IsFloating a) =>
   Acc (Array ix (a,a)) ->
   Acc (Array DIM1 (a,a)) ->
   Acc (Array ix a)
distanceMapPoints a b =
   A.fold1 min $
   outerVector
      (Exp.modify2 (expr,expr) (expr,expr) distance)
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
                    case Exp.unlift (expr:.expr:.expr) sh of
                       _z:.y:.x -> (4/) $ A.fromIntegral $ min x y
             in  imageByteFromFloat $ A.map (scale*) $
                 distanceMapPoints (pixelCoordinates sh) points
   in  \sh points ->
          distances sh $ array1FromList points


{- |
For every pixel
it computes the distance to the closest point on the image part boundary
which lies in any other image.
The rationale is that we want to fade an image out,
wherever is another image that can take over.
Such a closest point can either be a perpendicular point
at one of the image edges,
or it can be an image corner
or an intersection between this image border and another image border.
The first kind of points is computed by 'distanceMapContained'
and the second kind by 'distanceMapPoints'.
We simply compute the distances to all special points
and chose the minimal distance.
-}
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
                    case Exp.unlift (expr:.expr:.expr) sh of
                       _z:.y:.x -> (4/) $ A.fromIntegral $ min x y
             in  imageByteFromFloat $ A.map (scale*) $
                 distanceMap sh this others points
   in  \sh this others points ->
          distances sh this
             (array1FromList others)
             (array1FromList points)


distanceMapGamma ::
   (A.Elt a, A.IsFloating a) =>
   Exp a ->
   Exp DIM2 ->
   Exp ((a, a), (a, a), (Int, Int)) ->
   Acc (Array DIM1 ((a, a), (a, a), (Int, Int))) ->
   Acc (Array DIM1 (a, a)) ->
   Acc (Channel Z a)
distanceMapGamma gamma sh this others points =
   A.map (**gamma) $ distanceMap sh this others points


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
                    Exp.unlift ((expr,expr), (expr,expr), expr) this
             in  addToWeightedCanvas
                    (distanceMap (A.shape weightSum) this others points,
                     snd $ rotateStretchMove rot mov (unliftDim2 $ A.shape canvas) $
                     separateChannels $ imageFloatFromByte pic)
                    (weightSum,canvas)
   in  \this others points pic canvas ->
          update this
             (array1FromList others)
             (array1FromList points)
             pic canvas

updateWeightedCanvas ::
   Float ->
   ((Float,Float),(Float,Float),(Int,Int)) ->
   [((Float,Float),(Float,Float),(Int,Int))] ->
   [Point2 Float] ->
   Array DIM3 Word8 ->
   (Channel Z Float, Channel DIM1 Float) ->
   (Channel Z Float, Channel DIM1 Float)
updateWeightedCanvas =
   let distances = Run.with CUDA.run1 distanceMapGamma
       update =
          Run.with CUDA.run1 $
          \this pic dist (weightSum,canvas) ->
             let (rot, mov, _) =
                    Exp.unlift ((expr,expr), (expr,expr), expr) this
             in  addToWeightedCanvas
                    (dist,
                     snd $ rotateStretchMove rot mov (unliftDim2 $ A.shape canvas) $
                     separateChannels $ imageFloatFromByte pic)
                    (weightSum,canvas)
   in  \gamma this others points pic (weightSum,canvas) ->
          update this pic
             (distances gamma (A.arrayShape weightSum) this
                 (array1FromList others)
                 (array1FromList points))
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
                 (array1FromList others)
                 (array1FromList points),
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


processOverlap ::
   Option.Args ->
   [(Float, Array DIM3 Word8)] ->
   [((Int, (FilePath, ((Float, Float), Channel Z Float))),
     (Int, (FilePath, ((Float, Float), Channel Z Float))))] ->
   IO ([(Float, Float)], [((Float, Float), Array DIM3 Word8)])
processOverlap args picAngles pairs = do
   let opt = Option.option args
   let info = CmdLine.info (Option.verbosity opt)

   let padSize = Option.padSize opt
   let (maybeAllOverlapsShared, optimalOverlapShared) =
          case Just $ Z :. padSize :. padSize of
             Just padExtent ->
                (Nothing,
                 optimalOverlapBigFine padExtent (Option.minimumOverlap opt))
             Nothing ->
                let (rotHeights, rotWidths) =
                       unzip $
                       map (\(Z:.height:.width:._chans) -> (height, width)) $
                       map (A.arrayShape . snd) picAngles
                    maxSum2 sizes =
                       case List.sortBy (flip compare) sizes of
                          size0 : size1 : _ -> size0+size1
                          _ -> error "less than one picture - there should be no pairs"
                    padWidth  = ceilingPow2 $ maxSum2 rotWidths
                    padHeight = ceilingPow2 $ maxSum2 rotHeights
                    padExtent = Z :. padHeight :. padWidth
                in  (Just $ allOverlapsRun padExtent (Option.minimumOverlap opt),
                     optimalOverlap padExtent (Option.minimumOverlap opt))

   displacements <-
      fmap catMaybes $
      forM pairs $ \((ia,(pathA,(leftTopA,picA))), (ib,(pathB,(leftTopB,picB)))) -> do
         forM_ maybeAllOverlapsShared $ \allOverlapsShared -> when False $
            writeGrey (Option.quality opt)
               (printf "/tmp/%s-%s-score.jpeg"
                  (FilePath.takeBaseName pathA) (FilePath.takeBaseName pathB)) $
               allOverlapsShared picA picB

         let doffset@(dox,doy) = snd $ optimalOverlapShared picA picB
         let diff = overlapDifferenceRun doffset picA picB
         let overlapping = diff < Option.maximumDifference opt
         let d = (fromIntegral dox + fst leftTopA - fst leftTopB,
                  fromIntegral doy + snd leftTopA - snd leftTopB)
         info $
            printf "%s - %s, %s, difference %f%s\n" pathA pathB (show d) diff
               (if overlapping then "" else " unrelated -> ignoring")
         forM_ (Option.outputOverlap opt) $ \format ->
            writeImage (Option.quality opt)
               (printf format
                  (FilePath.takeBaseName pathA) (FilePath.takeBaseName pathB)) $
               composeOverlap doffset (picAngles!!ia, picAngles!!ib)
         return $ toMaybe overlapping ((ia,ib), d)

   let (poss, dps) =
          absolutePositionsFromPairDisplacements
             (length picAngles) displacements
   info "\nabsolute positions"
   info $ unlines $ map show poss

   info "\ncompare position differences with pair displacements"
   info $ unlines $
      zipWith
         (\(dpx,dpy) (dx,dy) ->
            printf "(%f,%f) (%f,%f)" dpx dpy dx dy)
         dps (map snd displacements)
   let (errdx,errdy) =
          mapPair (maximum,maximum) $ unzip $
          zipWith
             (\(dpx,dpy) (dx,dy) ->
                (abs $ dpx - realToFrac dx, abs $ dpy - realToFrac dy))
             dps (map snd displacements)

   info $
      "\n"
      ++
      printf "maximum horizontal error: %f\n" errdx
      ++
      printf "maximum vertical error: %f\n" errdy

   let picRots =
          map (mapFst (\angle -> (cos angle, sin angle))) picAngles
       floatPoss = map (mapPair (realToFrac, realToFrac)) poss

   return (floatPoss, picRots)


pairFromComplex :: (RealFloat a) => Complex a -> (a,a)
pairFromComplex z = (HComplex.realPart z, HComplex.imagPart z)

mapComplex :: (a -> b) -> Complex a -> Complex b
mapComplex f (r HComplex.:+ i)  =  f r HComplex.:+ f i


processOverlapRotate ::
   Option.Args ->
   [(Float, Array DIM3 Word8)] ->
   [((Int, (FilePath, ((Float, Float), Channel Z Float))),
     (Int, (FilePath, ((Float, Float), Channel Z Float))))] ->
   IO ([(Float, Float)], [((Float, Float), Array DIM3 Word8)])
processOverlapRotate args picAngles pairs = do
   let opt = Option.option args
   let info = CmdLine.info (Option.verbosity opt)

   let padSize = Option.padSize opt
   let stampSize = Option.stampSize opt
   let optimalOverlapShared =
          optimalOverlapBigMulti
             (Z :. padSize :. padSize)
             (Z :. stampSize :. stampSize)
             (Option.numberStamps opt)
             (Option.maximumDifference opt)
             (Option.minimumOverlap opt)

   displacements <-
      fmap concat $
      forM pairs $ \((ia,(pathA,(leftTopA,picA))), (ib,(pathB,(leftTopB,picB)))) -> do
         let add (x0,y0) (x1,y1) = (fromIntegral x0 + x1, fromIntegral y0 + y1)
         let correspondences =
                map
                   (\(score,pa,pb) ->
                      (score, ((ia, add pa leftTopA), (ib, add pb leftTopB)))) $
                optimalOverlapShared picA picB
         info $ printf "left-top: %s, %s" (show leftTopA) (show leftTopB)
         info $ printf "%s - %s" pathA pathB
         forM_ correspondences $ \(score, ((_ia,pa@(xa,ya)),(_ib,pb@(xb,yb)))) ->
            info $
               printf "%s ~ %s, (%f,%f), %f"
                  (show pa) (show pb) (xb-xa) (yb-ya) score
         return $ map snd correspondences

   let (posRots, dps) =
          layoutFromPairDisplacements (length picAngles) displacements
   info "\nabsolute positions and rotations: place, rotation (magnitude, phase)"
   info $ unlines $
      map
         (\(d,r) ->
            printf "%s, %s (%7.5f, %6.2f)" (show d) (show r)
               (HComplex.magnitude r) (HComplex.phase r * 180/pi))
         posRots

   info "\ncompare position differences with pair displacements"
   info $ unlines $
      zipWith
         (\(dpx,dpy) ((_ia,pa),(_ib,pb)) ->
            printf "(%f,%f) %s ~ %s" dpx dpy (show pa) (show pb))
         dps displacements

   let picRots =
          zipWith
             (\(angle,pic) rot ->
                (pairFromComplex $
                    HComplex.cis angle * mapComplex realToFrac rot,
                 pic))
             picAngles (map snd posRots)
       floatPoss = map (mapPair (realToFrac, realToFrac) . fst) posRots

   return (floatPoss, picRots)


process :: Option.Args -> IO ()
process args = do
   let paths = Option.inputs args
   let opt = Option.option args
   let notice = CmdLine.notice (Option.verbosity opt)
   let info = CmdLine.info (Option.verbosity opt)

   notice "\nfind rotation angles"
   picAngles <-
      forM paths $ \(imageOption, path) -> do
         pic <- readImage (Option.verbosity opt) path
         let maxAngle = Option.maximumAbsoluteAngle opt
         let angles =
                linearScale (Option.numberAngleSteps opt)
                   (-maxAngle, maxAngle)
         when False $ analyseRotations angles pic
         when False $ fourierTransformation opt path pic
         angle <-
            case Option.angle imageOption of
               Just angle -> return angle
               Nothing ->
                  if Option.radonTransform opt
                    then radonAngle (-maxAngle, maxAngle) pic
                    else return $ findOptimalRotation angles pic
         info $ printf "%s %f\176\n" path angle
         return (path, (angle*pi/180, pic))

   notice "\nfind relative placements"
   let rotated =
          map (mapSnd (prepareOverlapMatching (Option.smooth opt))) picAngles
   let prepared = map (snd . snd) rotated
   let pairs = do
          (a:as) <- tails $ zip [0..] rotated
          b <- as
          return (a,b)

   when False $ do
      notice "write fft"
      let pic0 : pic1 : _ = prepared
          size = (Z:.512:.1024 :: DIM2)
      writeGrey (Option.quality opt) "/tmp/padded.jpeg" $
         CUDA.run1
            (imageByteFromFloat .
             pad 0 (A.lift size)) $
         pic0
      writeGrey (Option.quality opt) "/tmp/spectrum.jpeg" $
         CUDA.run $ imageByteFromFloat $ A.map Complex.real $
         fft2DPlain CUFFT.forwardComplex $
         CUDA.run1 (A.map complexFromReal . pad 0 (A.lift size)) $
         pic0
      writeGrey (Option.quality opt) "/tmp/convolution.jpeg" $
         CUDA.run $ imageByteFromFloat $ A.map (0.000001*) $
         correlatePadded size (A.use pic0) (A.use pic1)

   (floatPoss, picRots) <-
      (if Option.finetuneRotate opt
         then processOverlapRotate
         else processOverlap)
            args (map snd picAngles) pairs

   notice "\ncompose all parts"
   let bbox (rot, pic) =
          case A.arrayShape pic of
             Z:.height:.width:._chans ->
                boundingBoxOfRotated rot
                   (fromIntegral width, fromIntegral height)
       ((canvasLeft,canvasRight), (canvasTop,canvasBottom)) =
          mapPair
             (mapPair (minimum, maximum) . unzip,
              mapPair (minimum, maximum) . unzip) $
          unzip $
          zipWith
             (\(mx,my) ->
                mapPair (mapPair ((mx+), (mx+)), mapPair ((my+), (my+))) . bbox)
             floatPoss picRots
       canvasWidth  = ceiling (canvasRight-canvasLeft)
       canvasHeight = ceiling (canvasBottom-canvasTop)
       canvasShape = Z :. canvasHeight :. canvasWidth
       movRotPics =
          zipWith
             (\(mx,my) (rot, pic) -> ((mx-canvasLeft, my-canvasTop), rot, pic))
             floatPoss picRots
   info $
      printf "canvas %f - %f, %f - %f\n"
         canvasLeft canvasRight canvasTop canvasBottom
   info $ printf "canvas size %d, %d\n" canvasWidth canvasHeight
   forM_ (Option.outputHard opt) $ \path ->
      writeImage (Option.quality opt) path $
      finalizeCanvas $
      foldl
         (\canvas (mov, rot, pic) -> updateCanvas rot mov pic canvas)
         (emptyCanvas (Z :. 3 :. canvasHeight :. canvasWidth))
         movRotPics

   notice "\ndistance maps"
   let geometries =
          map
             (\(mov, rot, pic) ->
                let Z:.height:.width:._chans = A.arrayShape pic
                    trans = rotateStretchMovePoint rot mov
                    widthf  = fromIntegral width
                    heightf = fromIntegral height
                    corner00 = trans (0,0)
                    corner10 = trans (widthf,0)
                    corner01 = trans (0,heightf)
                    corner11 = trans (widthf,heightf)
                    corners = [corner00, corner01, corner10, corner11]
                    edges =
                       [(corner00, corner10), (corner10, corner11),
                        (corner11, corner01), (corner01, corner00)]
                in  ((rot, mov, (width,height)), corners, edges))
             movRotPics

   let geometryRelations =
          flip map (removeEach geometries) $
             \((thisGeom, thisCorners, thisEdges), others) ->
                let intPoints = intersections thisEdges $ concatMap thd3 others
                    overlappingCorners =
                       filter
                          (\c ->
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
      when False $ do
         writeGrey (Option.quality opt)
            (printf "/tmp/%s-distance-box.jpeg" stem) $
            distanceMapBoxRun canvasShape thisGeom

         writeGrey (Option.quality opt)
            (printf "/tmp/%s-distance-contained.jpeg" stem) $
            distanceMapContainedRun canvasShape thisGeom otherGeoms

         writeGrey (Option.quality opt)
            (printf "/tmp/%s-distance-points.jpeg" stem) $
            distanceMapPointsRun canvasShape allPoints

      forM_ (Option.outputDistanceMap opt) $ \format ->
         writeGrey (Option.quality opt) (printf format stem) $
            distanceMapRun canvasShape thisGeom otherGeoms allPoints

   forM_ (Option.output opt) $ \path -> do
     notice "\nweighted composition"
     writeImage (Option.quality opt) path $
      finalizeWeightedCanvas $
      foldl
         (\canvas ((thisGeom, otherGeoms, allPoints), (_rot, pic)) ->
            updateWeightedCanvas (Option.distanceGamma opt)
               thisGeom otherGeoms allPoints pic canvas)
         (emptyWeightedCanvas (Z :. 3 :. canvasHeight :. canvasWidth))
         (zip geometryRelations picRots)

main :: IO ()
main = process =<< Option.get
