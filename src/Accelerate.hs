{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Option
import qualified State

import qualified Arithmetic as Arith
import qualified Degree
import LinearAlgebra (
   absolutePositionsFromPairDisplacements, fixAtLeastOnePosition,
   layoutFromPairDisplacements, fixAtLeastOneAnglePosition,
   )
import Degree (Degree(Degree), getDegree)
import Arithmetic (
   Point2,
   rotateStretchMovePoint,
   rotateStretchMoveBackPoint,
   boundingBoxOfRotated,
   linearIp,
   cubicIp,
   smooth3,
   projectPerp,
   distance,
   linearScale,
   divUp,
   guardedPairs,
   maximum0,
   )

import qualified Data.Array.Accelerate.Fourier.Real as FourierReal
import qualified Data.Array.Accelerate.CUFFT.Single as CUFFT
import qualified Data.Array.Accelerate.Data.Complex as AComplex
import qualified Data.Array.Accelerate.Data.Bits as ABits
import qualified Data.Array.Accelerate.LLVM.PTX as CUDA
import qualified Data.Array.Accelerate.IO as AIO
import qualified Data.Array.Accelerate.LinearAlgebra as LinAlg
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
           (:.)((:.)), Z(Z), Any(Any), All(All), (?), (!), )

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Data.Complex as Complex

import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV

import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Shell.Utility.Log as CmdLine
import qualified Shell.Utility.Verbosity as Verbosity
import Shell.Utility.Verbosity (Verbosity)
import Text.Printf (printf)

import Control.Monad.HT (void)
import Control.Monad (liftM2, when, join)
import Control.Applicative ((<$>))

import qualified Data.Foldable as Fold
import qualified Data.List.Key as Key
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Maybe.HT (toMaybe)
import Data.Maybe (mapMaybe, isNothing)
import Data.List.HT (mapAdjacent)
import Data.Traversable (forM)
import Data.Foldable (forM_, foldMap)
import Data.Tuple.HT (mapPair, mapFst, mapSnd, mapThd3)
import Data.Word (Word8)

import System.IO.Unsafe (unsafePerformIO)


type ColorImage8 = Array DIM3 Word8

readImage :: Verbosity -> FilePath -> IO ColorImage8
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
                     (Z :. Pic.imageHeight pic :. Pic.imageWidth pic :. 3) dat
            _ -> ioError $ userError "unsupported image type"

writeImage :: Int -> FilePath -> ColorImage8 -> IO ()
writeImage quality path arr = do
   let (Z :. height :. width :. 3) = A.arrayShape arr
   Pic.saveJpgImage quality path $ Pic.ImageYCbCr8 $
      Pic.Image {
         Pic.imageWidth = width,
         Pic.imageHeight = height,
         Pic.imageData = AIO.toVectors arr
      }

writeGrey :: Int -> FilePath -> Array DIM2 Word8 -> IO ()
writeGrey quality path arr = do
   let (Z :. height :. width) = A.arrayShape arr
   Pic.saveJpgImage quality path $ Pic.ImageY8 $
      Pic.Image {
         Pic.imageWidth = width,
         Pic.imageHeight = height,
         Pic.imageData = AIO.toVectors arr
      }

colorImageExtent :: ColorImage8 -> (Int, Int)
colorImageExtent pic =
   case A.arrayShape pic of Z:.height:.width:._chans -> (width, height)

imageFloatFromByte ::
   (A.Shape sh, A.Floating a, A.FromIntegral Word8 a) =>
   Acc (Array sh Word8) -> Acc (Array sh a)
imageFloatFromByte = A.map ((/255) . A.fromIntegral)

imageByteFromFloat ::
   (A.Shape sh, A.RealFloat a) =>
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


fastRound :: (A.Elt i, A.IsIntegral i, A.RealFloat a) => Exp a -> Exp i
fastRound x = A.floor (x+0.5)

floatArray :: Acc (Array sh Float) -> Acc (Array sh Float)
floatArray = id


splitFraction ::
   (A.RealFloat a, A.FromIntegral Int a) => Exp a -> (Exp Int, Exp a)
splitFraction x =
   let i = A.floor x
   in  (i, x - A.fromIntegral i)


target :: CUDA.PTX
target = unsafePerformIO CUFFT.getBestTarget

cudaRun :: (A.Arrays a) => Acc a -> a
cudaRun = CUDA.runWith target

cudaRun1 :: (A.Arrays a, A.Arrays b) => (Acc a -> Acc b) -> a -> b
cudaRun1 = CUDA.run1With target


type Channel ix = Array (ix :. Int :. Int)
type Plane = Channel Z

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
   in  arr ! A.lift (ix :. yc :. xc)

indexFrac ::
   (A.Slice ix, A.Shape ix, A.RealFloat a, A.FromIntegral Int a) =>
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
   (A.Floating a, A.FromIntegral Int a) =>
   (Exp a, Exp a) ->
   (Exp a, Exp a) ->
   (Exp Int, Exp Int) ->
   Acc (Channel Z (a, a))
rotateStretchMoveCoords rot mov (width,height) =
   let trans = rotateStretchMoveBackPoint rot mov
   in  A.generate (A.lift $ Z:.height:.width) $ \p ->
          let (_z :. ydst :. xdst) = unliftDim2 p
          in  A.lift $ trans (A.fromIntegral xdst, A.fromIntegral ydst)

inBox ::
   (A.Num a, A.Ord a) =>
   (Exp a, Exp a) ->
   (Exp a, Exp a) ->
   Exp Bool
inBox (width,height) (x,y) =
   0 A.<= x  A.&&  x A.< width  A.&&  0 A.<= y  A.&&  y A.< height

validCoords ::
   (A.RealFloat a) =>
   (Exp Int, Exp Int) ->
   Acc (Channel Z (a, a)) ->
   Acc (Channel Z Bool)
validCoords (width,height) =
   A.map $ A.lift1 $ \(x,y) ->
      inBox (width,height) (fastRound x, fastRound y)

replicateChannel ::
   (A.Slice ix, A.Shape ix, A.Elt a) =>
   Exp (ix :. Int :. Int) -> Acc (Channel Z a) -> Acc (Channel ix a)
replicateChannel = LinAlg.extrudeMatrix . A.indexTail . A.indexTail

{- |
@rotateStretchMove rot mov@
first rotate and stretches the image according to 'rot'
and then moves the picture.
-}
rotateStretchMove ::
   (A.Slice ix, A.Shape ix, A.RealFloat a, A.FromIntegral Int a) =>
   (Exp a, Exp a) ->
   (Exp a, Exp a) ->
   ExpDIM2 ix -> Acc (Channel ix a) ->
   (Acc (Channel Z Bool), Acc (Channel ix a))
rotateStretchMove rot mov sh arr =
   let (_chansDst :. heightDst :. widthDst) = sh
       (_chansSrc :. heightSrc :. widthSrc) = unliftDim2 $ A.shape arr
       coords = rotateStretchMoveCoords rot mov (widthDst, heightDst)

   in  (validCoords (widthSrc, heightSrc) coords,
        Arrange.mapWithIndex
           (\ix coord ->
              let (chan :. _ydst :. _xdst) = unliftDim2 ix
                  (xsrc,ysrc) = A.unlift coord
              in  indexFrac arr (chan :. ysrc :. xsrc))
           (replicateChannel (A.lift sh) coords))


rotateLeftTop ::
   (A.Slice ix, A.Shape ix, A.RealFloat a, A.FromIntegral Int a) =>
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
   (A.Slice ix, A.Shape ix, A.RealFloat a, A.FromIntegral Int a) =>
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
   Degree Float -> ColorImage8 -> (ColorImage8, Array DIM1 Float)
rotateHistogram =
   let rot =
          Run.with cudaRun1 $ \orient arr ->
             let rotated =
                    rotate orient $
                    separateChannels $ imageFloatFromByte arr
             in  (imageByteFromFloat $ interleaveChannels rotated,
                  rowHistogram rotated)
   in  \angle arr -> rot (Degree.cis angle) arr


analyseRotations :: [Degree Float] -> ColorImage8 -> IO ()
analyseRotations angles pic = do
   histograms <-
      forM angles $ \degree -> do
         let (rotated, histogram) = rotateHistogram degree pic
         let stem = printf "rotated%+07.2f" $ getDegree degree
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



differentiate :: (A.Num a) => Acc (Array DIM1 a) -> Acc (Array DIM1 a)
differentiate arr =
   let size = A.unindex1 $ A.shape arr
   in  A.generate (A.index1 (size-1)) $ \i ->
          arr ! (A.index1 $ A.unindex1 i + 1) - arr ! i

scoreRotation :: Degree Float -> ColorImage8 -> Float
scoreRotation =
   let rot =
          Run.with cudaRun1 $ \orient arr ->
             A.sum $ A.map (^(2::Int)) $ differentiate $ rowHistogram $
             rotate orient $ separateChannels $ imageFloatFromByte arr
   in  \angle arr -> Acc.the $ rot (Degree.cis angle) arr

findOptimalRotation :: [Degree Float] -> ColorImage8 -> Degree Float
findOptimalRotation angles pic =
   Key.maximum (flip scoreRotation pic) angles


magnitudeSqr :: (A.Num a) => Exp (Complex a) -> Exp a
magnitudeSqr =
   Exp.modify (expr:+expr) $ \(r:+i) -> r*r+i*i

fourierTransformationRun :: ColorImage8 -> IO (Array DIM2 Word8)
fourierTransformationRun pic = do
   let (shape@(Z:.height:.width):._) = A.arrayShape pic
   plan <- CUFFT.plan2D target CUFFT.forwardReal shape
   let trans =
          Run.with cudaRun1 $ \arr ->
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
             brightnessPlane $ separateChannels $
             imageFloatFromByte arr
   return $ trans pic

fourierTransformation :: Option.Option -> FilePath -> ColorImage8 -> IO ()
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
   (A.RealFloat a, A.FromIntegral Int a) =>
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
              z0 = weighted ! A.lift (Z :. y :. mod xi width)
              z1 = weighted ! A.lift (Z :. y :. mod (xi+1) width)
          in  linearIp (z0,z1) frac

radonAngle ::
   (Degree Float, Degree Float) -> ColorImage8 -> IO (Degree Float)
radonAngle (minAngle,maxAngle) pic = do
   let (shape@(Z :. height :. _width):._) = A.arrayShape pic
   plan <- CUFFT.plan2D target CUFFT.forwardReal shape
   let height2 = fromIntegral (div height 2)
   let slope w = tan (Degree.toRadian w) * height2
   let minX = floor $ slope minAngle
   let maxX = ceiling $ slope maxAngle
   let angle s = Degree.fromRadian $ atan (s/height2)
   let trans =
          Run.with cudaRun1 $ \arr ->
             A.map A.snd $ argmaximum $
             Arrange.mapWithIndex (\ix s -> A.lift (s, A.unindex1 ix)) $
             scoreSlopes (A.constant minX, A.constant maxX) $
             CUFFT.transform plan $
             brightnessPlane $ separateChannels $
             imageFloatFromByte arr
   return $ angle $ fromIntegral $ Acc.the (trans pic) + minX


rotateManifest :: Degree Float -> ColorImage8 -> Array DIM3 Float
rotateManifest =
   let rot =
          Run.with cudaRun1 $ \orient arr ->
             rotate orient $ separateChannels $ imageFloatFromByte arr
   in  \angle arr -> rot (Degree.cis angle) arr


prepareOverlapMatching ::
   Int -> (Degree Float, ColorImage8) -> ((Float,Float), Plane Float)
prepareOverlapMatching =
   let rot =
          Run.with cudaRun1 $ \radius orient arr ->
             rotateLeftTop orient $
             (if True
                then highpass radius
                else removeDCOffset) $
             brightnessPlane $ separateChannels $ imageFloatFromByte arr
   in  \radius (angle, arr) ->
          mapFst (mapPair (Acc.the, Acc.the)) $
          rot radius (Degree.cis angle) arr


ceilingPow2 :: Exp Int -> Exp Int
ceilingPow2 n =
   ABits.setBit 0 $ A.ceiling $ logBase 2 (fromIntegral n :: Exp Double)

pad ::
   (A.Elt a) =>
   Exp a -> Exp DIM2 -> Acc (Channel Z a) -> Acc (Channel Z a)
pad a sh arr =
   let (height, width) = A.unlift $ A.unindex2 $ A.shape arr
   in  A.generate sh $ \p ->
          let (y, x) = A.unlift $ A.unindex2 p
          in  (y A.< height  A.&&  x A.< width)
              ?
              (arr ! A.index2 y x, a)

mulConj ::
   (A.RealFloat a, A.FromIntegral Int a) =>
   Exp (Complex a) -> Exp (Complex a) -> Exp (Complex a)
mulConj x y = x * AComplex.conjugate y


fft2DGen ::
   (A.Elt e, CUFFT.Real e) =>
   CUFFT.Mode DIM2 e a b -> DIM2 -> CUFFT.Transform DIM2 a b
fft2DGen mode sh =
   CUFFT.transform $ unsafePerformIO $ CUFFT.plan2D target mode sh

fft2DPlain ::
   (A.Elt e, CUFFT.Real e, A.Elt a, A.Elt b) =>
   CUFFT.Mode DIM2 e a b ->
   Channel Z a -> Acc (Channel Z b)
fft2DPlain mode arr =
   A.use $ cudaRun1 (fft2DGen mode $ A.arrayShape arr) arr

fft2D ::
   (A.Elt e, CUFFT.Real e, A.Elt a, A.Elt b) =>
   CUFFT.Mode DIM2 e a b ->
   Int -> Int ->
   Acc (Channel Z a) -> Acc (Channel Z b)
fft2D mode width height = fft2DGen mode (Z:.height:.width)


correlateImpossible ::
   (A.Elt a, CUFFT.Real a) =>
   Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
correlateImpossible x y =
   let (heightx, widthx) = A.unlift $ A.unindex2 $ A.shape x
       (heighty, widthy) = A.unlift $ A.unindex2 $ A.shape y
       width  = ceilingPow2 $ widthx  + widthy
       height = ceilingPow2 $ heightx + heighty
       sh = A.index2 height width
       forward z = fft2DPlain CUFFT.forwardReal $ cudaRun $ pad 0 sh z
   in  fft2DPlain CUFFT.inverseReal $ cudaRun $
       A.zipWith mulConj (forward x) (forward y)


removeDCOffset ::
   (A.Floating a, A.FromIntegral Int a) =>
   Acc (Channel Z a) -> Acc (Channel Z a)
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
   (A.RealFloat a, A.FromIntegral Int a) =>
   Acc (Array DIM2 (Complex a)) -> Acc (Array DIM2 (Complex a))
clearDCCoefficient arr =
   A.generate (A.shape arr) $ \p ->
      let (_z:.y:.x) = unliftDim2 p
      in  x A.== 0  A.||  y A.== 0 ? (0, arr!p)


lowpass, highpass ::
   (A.Floating a) => Exp Int -> Acc (Channel Z a) -> Acc (Channel Z a)
lowpass count =
   Loop.nest count $
      A.stencil (\(a,m,b) -> smooth3 (smooth3 a, smooth3 m, smooth3 b)) A.clamp

highpass count arr =
   A.zipWith (-) arr $ lowpass count arr


correlatePaddedSimple ::
   (A.Elt a, CUFFT.Real a) =>
   DIM2 -> Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
correlatePaddedSimple sh@(Z :. height :. width) =
   let forward = fft2D CUFFT.forwardReal width height . pad 0 (A.lift sh)
       inverse = fft2D CUFFT.inverseReal width height
   in  \ x y -> inverse $ A.zipWith mulConj (forward x) (forward y)


{-
This is more efficient than 'correlatePaddedSimple'
since it needs only one complex forward Fourier transform,
where 'correlatePaddedSimple' needs two real transforms.
Especially for odd sizes
two real transforms are slower than a complex transform.
For the analysis part,
perform two real-valued Fourier transforms using one complex-valued transform.
Afterwards we untangle the superposed spectra.
-}
correlatePadded ::
   (A.Elt a, CUFFT.Real a) =>
   DIM2 -> Acc (Channel Z a) -> Acc (Channel Z a) -> Acc (Channel Z a)
correlatePadded sh@(Z :. height :. width) =
   let forward = fft2D CUFFT.forwardComplex width height
       inverse = fft2D CUFFT.inverseReal width height
   in  \ a b ->
          inverse $ A.map (A.uncurry mulConj) $
          FourierReal.untangleSpectra2d $ forward $
          pad 0 (A.lift sh) $
          A.zipWith (Exp.modify2 expr expr (:+)) a b


wrap :: Exp Int -> Exp Int -> Exp Int -> Exp Int
wrap size split c = c A.< split ? (c, c-size)

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


{- |
Set all scores to zero within a certain border.
Otherwise the matching algorithm will try to match strong bars at the borders
that are actually digitalization artifacts.
-}
minimumOverlapScores ::
   (A.Floating a, A.IsScalar a) =>
   ((Exp Int, Exp Int) -> Exp a -> Exp a) ->
   Exp Int -> (Exp Int, Exp Int) -> (Exp Int, Exp Int) ->
   Acc (Channel Z (a, (Int, Int))) ->
   Acc (Channel Z (a, (Int, Int)))
minimumOverlapScores weight minOverlap (widtha,heighta) (widthb,heightb) =
   A.map
       (Exp.modify (expr,(expr,expr)) $ \(v, dp@(dx,dy)) ->
          let clipWidth  = min widtha  (widthb  + dx) - max 0 dx
              clipHeight = min heighta (heightb + dy) - max 0 dy
          in  ((clipWidth A.>= minOverlap   A.&&   clipHeight A.>= minOverlap)
               ?
               (weight (clipWidth, clipHeight) v, 0),
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
                   then \(clipWidth, clipHeight) v ->
                     v / (A.fromIntegral clipWidth * A.fromIntegral clipHeight)
                   else const id
          in  minimumOverlapScores weight minOverlap
                 (widtha, heighta) (widthb, heightb) $
              attachDisplacements
                 (half $ A.lift width - widthb + widtha)
                 (half $ A.lift height - heightb + heighta) $
              correlate a b


allOverlapsRun ::
   DIM2 -> Float -> Plane Float -> Plane Float -> Plane Word8
allOverlapsRun padExtent =
   Run.with cudaRun1 $ \minOverlap picA picB ->
      imageByteFromFloat $
      -- A.map (2*) $
      A.map (0.0001*) $
      A.map A.fst $ allOverlaps padExtent minOverlap picA picB

optimalOverlap ::
   DIM2 -> Float -> Plane Float -> Plane Float -> (Float, (Int, Int))
optimalOverlap padExtent =
   let run =
          Run.with cudaRun1 $ \minimumOverlap a b ->
          argmaximum $ allOverlaps padExtent minimumOverlap a b
   in  \overlap a b -> Acc.the $ run overlap a b


shrink ::
   (A.Slice ix, A.Shape ix, A.Floating a, A.FromIntegral Int a) =>
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


type GenDIM2 a = Z :. a :. a

-- cf. Arithmetic.minimumOverlapAbsFromPortion
minimumOverlapAbsFromPortion ::
   (Num a, Ord i) => (a -> i, i -> a) -> a -> (i, i) -> i
minimumOverlapAbsFromPortion
   (afloor, fromInt) minOverlapPortion (width, height) =
      afloor $ minOverlapPortion * fromInt (min width height)

shrinkFactors ::
   (Num a, Integral i) =>
   (a -> i, i -> a) ->
   DIM2 -> a -> GenDIM2 i -> GenDIM2 i -> GenDIM2 i
shrinkFactors methods (Z:.heightPad:.widthPad) minOverlapPortion
   (Z :. heighta :. widtha) (Z :. heightb :. widthb) =
      let minOverlap =
            minimumOverlapAbsFromPortion methods minOverlapPortion
               (min widtha widthb, min heighta heightb)
          yk = divUp (heighta+heightb-minOverlap) $ fromIntegral heightPad
          xk = divUp (widtha +widthb -minOverlap) $ fromIntegral widthPad
      in  Z :. yk :. xk

{-
Reduce image sizes below the padExtent before matching images.
-}
optimalOverlapBig ::
   DIM2 -> Float -> Plane Float -> Plane Float -> (Float, (Int, Int))
optimalOverlapBig padExtent =
   let run =
          Run.with cudaRun1 $ \minimumOverlap a b ->
             let factors@(_z:.yk:.xk) =
                    shrinkFactors (A.floor, A.fromIntegral) padExtent
                       minimumOverlap
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
   DIM2 -> Float -> Plane Float -> Plane Float -> (Float, (Int, Int))
optimalOverlapBigFine padExtent@(Z:.heightPad:.widthPad) =
   let overlaps = allOverlaps padExtent
       run =
          Run.with cudaRun1 $ \minimumOverlap a b ->
             let shapeA = A.unlift $ A.shape a
                 shapeB = A.unlift $ A.shape b
                 factors@(_z:.yk:.xk) =
                    shrinkFactors (A.floor, A.fromIntegral) padExtent
                       minimumOverlap shapeA shapeB
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
   Float -> Maybe Float -> Plane Float -> Plane Float ->
   [(Float, (Int, Int), (Int, Int))]
optimalOverlapBigMulti padExtent (Z:.heightStamp:.widthStamp) numCorrs =
   let overlapShrunk =
          Run.with cudaRun1 $
          \minimumOverlap factors a b ->
             argmaximum $
             allOverlaps padExtent minimumOverlap
                (shrink factors a) (shrink factors b)
       diffShrunk =
          Run.with cudaRun1 $
          \shrunkd factors a b ->
             overlapDifference shrunkd
                (shrink factors a) (shrink factors b)

       allOverlapsFine = allOverlaps (Z :. 2*heightStamp :. 2*widthStamp)
       overlapFine =
          Run.with cudaRun1 $
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

   in  \minimumOverlap mMaximumDiff a b ->
          let factors@(Z:.yk:.xk) =
                 shrinkFactors (floor, fromIntegral) padExtent
                    minimumOverlap (A.arrayShape a) (A.arrayShape b)

              (_score, shrunkd@(shrunkdx, shrunkdy)) =
                 Acc.the $ overlapShrunk minimumOverlap factors a b

              coarsedx = shrunkdx * xk
              coarsedy = shrunkdy * yk
              coarsed = (coarsedx,coarsedy)

              doesOverlap =
                 case mMaximumDiff of
                    Just maximumDiff ->
                       maximumDiff > Acc.the (diffShrunk shrunkd factors a b)
                    Nothing -> True

              ((leftOverlap, topOverlap),
               (rightOverlap, bottomOverlap),
               (widthOverlap, heightOverlap))
                 = overlappingArea (A.arrayShape a) (A.arrayShape b) coarsed

              widthStampClip = min widthOverlap widthStamp
              heightStampClip = min heightOverlap heightStamp

          in  (if doesOverlap then id else const []) $
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
   (A.Slice ix, A.Shape ix, A.Floating a, A.FromIntegral Int a) =>
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
   Plane Float -> Plane Float -> Float
overlapDifferenceRun =
   let diff = Run.with cudaRun1 overlapDifference
   in  \d a b -> Acc.the $ diff d a b


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
              inPicA = inBox (widtha,heighta) (xa,ya)
              inPicB = inBox (widthb,heightb) (xb,yb)
          in  inPicA ?
                 (inPicB ? ((a!pa + b!pb)/2, a!pa),
                  inPicB ? (b!pb, 0))

composeOverlap ::
   (Int, Int) ->
   ((Degree Float, ColorImage8), (Degree Float, ColorImage8)) ->
   ColorImage8
composeOverlap =
   let rotat (rot,pic) =
          rotate rot $ separateChannels $ imageFloatFromByte pic
   in  (\f d (a,b) -> f d (mapFst Degree.cis a, mapFst Degree.cis b)) $
       Run.with cudaRun1 $
       \(dx,dy) (a,b) ->
          imageByteFromFloat $ interleaveChannels $
          overlap2 (dx, dy) (rotat a, rotat b)


emptyCountCanvas ::
   (A.Slice ix, A.Shape ix) =>
   ix :. Int :. Int ->
   (Plane Int, Channel ix Float)
emptyCountCanvas =
   Run.with cudaRun1 $ \sh ->
      let (_ix :. height :. width) = unliftDim2 sh
      in  (A.fill (A.lift $ Z:.height:.width) 0,
           A.fill sh 0)


addToCountCanvas ::
   (A.Slice ix, A.Shape ix, A.Num a, A.FromIntegral Int a) =>
   (Acc (Plane Bool), Acc (Channel ix a)) ->
   (Acc (Plane Int),  Acc (Channel ix a)) ->
   (Acc (Plane Int),  Acc (Channel ix a))
addToCountCanvas (mask, pic) (count, canvas) =
   (A.zipWith (+) (A.map A.boolToInt mask) count,
    A.zipWith (+) canvas $ A.zipWith (*) pic $
    replicateChannel (A.shape pic) $
    A.map (A.fromIntegral . A.boolToInt) mask)

updateCountCanvas ::
   ((Float,Float), (Float,Float), ColorImage8) ->
   (Plane Int, Channel DIM1 Float) ->
   (Plane Int, Channel DIM1 Float)
updateCountCanvas =
   Run.with cudaRun1 $
   \(rot, mov, pic) (count,canvas) ->
      addToCountCanvas
         (rotateStretchMove rot mov (unliftDim2 $ A.shape canvas) $
          separateChannels $ imageFloatFromByte pic)
         (count,canvas)

finalizeCountCanvas :: (Plane Int, Channel DIM1 Float) -> ColorImage8
finalizeCountCanvas =
   Run.with cudaRun1 $
   \(count, canvas) ->
      imageByteFromFloat $ interleaveChannels $
      A.zipWith (/) canvas $
      replicateChannel (A.shape canvas) $
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
   (A.Shape ix, A.Ord a) =>
   LinAlg.Vector ix (Bool, a) ->
   LinAlg.Scalar ix (Bool, a)
maskedMinimum = A.fold1 (maybePlus min)

maskedMaximum ::
   (A.Shape ix, A.Ord a) =>
   LinAlg.Vector ix (Bool, a) ->
   LinAlg.Scalar ix (Bool, a)
maskedMaximum = A.fold1 (maybePlus max)


project ::
   (A.RealFloat a, A.FromIntegral Int a) =>
   Point2 (Exp a) ->
   (Point2 (Exp a), Point2 (Exp a)) ->
   (Exp Bool, Point2 (Exp a))
project x ab =
   let (r, y) = projectPerp x ab
   in  (0 A.<= r  A.&&  r A.<= 1, y)


distanceMapEdges ::
   (A.RealFloat a, A.FromIntegral Int a) =>
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
   DIM2 -> Array DIM1 ((Float,Float),(Float,Float)) -> Plane Word8
distanceMapEdgesRun =
   Run.with cudaRun1 $ \sh ->
      imageByteFromFloat . A.map (0.01*) . distanceMapEdges sh

type Geometry a = Arith.Geometry Int a

distanceMapBox ::
   (A.RealFloat a, A.FromIntegral Int a) =>
   Exp DIM2 ->
   Exp (Geometry a) ->
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

distanceMapBoxRun :: DIM2 -> Geometry Float -> Plane Word8
distanceMapBoxRun =
   Run.with cudaRun1 $ \sh geom ->
      scaleDistanceMapGeom geom $
      A.map (Exp.modify (expr,expr) $ \(valid, dist) -> valid ? (dist, 0)) $
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
   (A.RealFloat a) =>
   Acc (Array DIM1 (Geometry a)) ->
   Acc (Array DIM3 (a,a)) ->
   Acc (Array DIM3 Bool)
containedAnywhere geoms arr =
   A.fold1 (A.||) $
   breakFusion $
   outerVector
      (Exp.modify2 (expr,expr) ((expr,expr),(expr,expr),(expr,expr)) $
       \(xdst,ydst) (rot, mov, extent) ->
          let (xsrc,ysrc) = rotateStretchMoveBackPoint rot mov (xdst,ydst)
          in  inBox extent (fastRound xsrc, fastRound ysrc))
      arr geoms


distanceMapContained ::
   (A.RealFloat a, A.FromIntegral Int a) =>
   Exp DIM2 ->
   Exp (Geometry a) ->
   Acc (Array DIM1 (Geometry a)) ->
   Acc (Channel Z a)
distanceMapContained sh this others =
   let distMap = separateDistanceMap $ distanceMapBox sh this
       contained = containedAnywhere others $ A.map (A.snd . A.snd) distMap
   in  A.map (Exp.modify (expr,expr) $ \(valid, dist) -> valid ? (dist, 0)) $
       maskedMinimum $
       A.zipWith
          (Exp.modify2 expr (expr,(expr,expr)) $ \c (b,(dist,_)) ->
             (c A.&& b, dist))
          contained distMap

distanceMapContainedRun ::
   DIM2 -> Geometry Float -> [Geometry Float] -> Plane Word8
distanceMapContainedRun =
   let distances =
          Run.with cudaRun1 $
          \sh this -> scaleDistanceMapGeom this . distanceMapContained sh this
   in  \sh this others -> distances sh this $ array1FromList others

scaleDistanceMapGeom ::
   (A.RealFloat a, A.FromIntegral Int a, A.Elt b, A.Shape ix) =>
   Exp (Geometry b) -> Acc (Array ix a) -> Acc (Array ix Word8)
scaleDistanceMapGeom this =
   let scale = (4/) $ A.fromIntegral $ A.uncurry min $ Exp.thd3 this
   in  imageByteFromFloat . A.map (scale*)


pixelCoordinates ::
   (A.RealFloat a, A.FromIntegral Int a) => Exp DIM2 -> Acc (Channel Z (a,a))
pixelCoordinates sh =
   A.generate sh $ Exp.modify (expr:.expr:.expr) $ \(_z:.y:.x) ->
      (A.fromIntegral x, A.fromIntegral y)

distanceMapPoints ::
   (A.Slice ix, A.Shape ix, A.RealFloat a) =>
   Acc (Array ix (a,a)) ->
   Acc (Array DIM1 (a,a)) ->
   Acc (Array ix a)
distanceMapPoints a b =
   A.fold1 min $
   outerVector
      (Exp.modify2 (expr,expr) (expr,expr) distance)
      a b

distanceMapPointsRun :: DIM2 -> [Point2 Float] -> Plane Word8
distanceMapPointsRun =
   let distances =
          Run.with cudaRun1 $
          \sh -> scaleDistanceMap . distanceMapPoints (pixelCoordinates sh)
   in  \sh points -> distances sh $ array1FromList points

scaleDistanceMap ::
   (A.RealFloat a, A.FromIntegral Int a) =>
   Acc (Channel Z a) -> Acc (Channel Z Word8)
scaleDistanceMap arr =
   let scale =
         case Exp.unlift (expr:.expr:.expr) (A.shape arr) of
            _z:.y:.x -> 4 / A.fromIntegral (min x y)
   in  imageByteFromFloat $ A.map (scale*) arr


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
   (A.RealFloat a, A.FromIntegral Int a) =>
   Exp DIM2 ->
   Exp (Geometry a) ->
   Acc (Array DIM1 (Geometry a)) ->
   Acc (Array DIM1 (a, a)) ->
   Acc (Channel Z a)
distanceMap sh this others points =
   A.zipWith min
      (distanceMapContained sh this others)
      (distanceMapPoints (pixelCoordinates sh) points)

distanceMapRun ::
   DIM2 ->
   Geometry Float ->
   [Geometry Float] ->
   [Point2 Float] ->
   Plane Word8
distanceMapRun =
   let distances =
          Run.with cudaRun1 $
          \sh this others -> scaleDistanceMap . distanceMap sh this others
   in  \sh this others points ->
          distances sh this
             (array1FromList others)
             (array1FromList points)


distanceMapGamma ::
   (A.RealFloat a, A.FromIntegral Int a) =>
   Exp a ->
   Exp DIM2 ->
   Exp (Geometry a) ->
   Acc (Array DIM1 (Geometry a)) ->
   Acc (Array DIM1 (a, a)) ->
   Acc (Channel Z a)
distanceMapGamma gamma sh this others points =
   A.map (**gamma) $ distanceMap sh this others points


emptyWeightedCanvas ::
   (A.Slice ix, A.Shape ix) =>
   ix :. Int :. Int ->
   (Plane Float, Channel ix Float)
emptyWeightedCanvas =
   Run.with cudaRun1 $ \sh ->
      let (_ix :. height :. width) = unliftDim2 sh
      in  (A.fill (A.lift $ Z:.height:.width) 0,
           A.fill sh 0)


addToWeightedCanvas ::
   (A.Slice ix, A.Shape ix, A.Num a) =>
   (Acc (Channel Z a), Acc (Channel ix a)) ->
   (Acc (Channel Z a), Acc (Channel ix a)) ->
   (Acc (Channel Z a), Acc (Channel ix a))
addToWeightedCanvas (weight, pic) (weightSum, canvas) =
   (A.zipWith (+) weight weightSum,
    A.zipWith (+) canvas $ A.zipWith (*) pic $
    replicateChannel (A.shape pic) weight)

-- launch timeout
updateWeightedCanvasMerged ::
   Geometry Float ->
   [Geometry Float] ->
   [Point2 Float] ->
   ColorImage8 ->
   (Plane Float, Channel DIM1 Float) ->
   (Plane Float, Channel DIM1 Float)
updateWeightedCanvasMerged =
   let update =
          Run.with cudaRun1 $
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
   Geometry Float ->
   [Geometry Float] ->
   [Point2 Float] ->
   ColorImage8 ->
   (Plane Float, Channel DIM1 Float) ->
   (Plane Float, Channel DIM1 Float)
updateWeightedCanvas =
   let distances = Run.with cudaRun1 distanceMapGamma
       update =
          Run.with cudaRun1 $
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
   Geometry Float ->
   [Geometry Float] ->
   [Point2 Float] ->
   ColorImage8 ->
   (Plane Float, Channel DIM1 Float) ->
   (Plane Float, Channel DIM1 Float)
updateWeightedCanvasSplit =
   let update = Run.with cudaRun1 addToWeightedCanvas
       distances = Run.with cudaRun1 distanceMap
       rotated =
          Run.with cudaRun1 $
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
   (Plane Float, Channel DIM1 Float) -> ColorImage8
finalizeWeightedCanvas =
   Run.with cudaRun1 $
   \(weightSum, canvas) ->
      imageByteFromFloat $ interleaveChannels $
      A.zipWith (/) canvas $
      replicateChannel (A.shape canvas) weightSum


data
   Picture param =
      Picture {
         picPath :: FilePath,
         picParam :: param,
         picColored :: (Degree Float, ColorImage8),
         picPlane :: ((Float, Float), Plane Float)
      }

mapPicParam :: (a -> b) -> Picture a -> Picture b
mapPicParam f pic = pic{picParam = f $ picParam pic}


processOverlap ::
   Option.Args ->
   IO ([FilePath], [(Degree Float, ColorImage8)],
       [((Float, Float), Complex Float)])
processOverlap args = do
   let opt = Option.option args
   let info = CmdLine.info (Option.verbosity opt)

   pics <-
      map (mapPicParam (\(State.NoAngleCorrection, pos) -> pos)) <$>
      processRotation args
   let padSize = Option.padSize opt
   let (maybeAllOverlapsShared, optimalOverlapShared) =
          case Just $ Z :. padSize :. padSize of
             Just padExtent ->
                (Nothing,
                 optimalOverlapBigFine padExtent (Option.minimumOverlap opt))
             Nothing ->
                let (padWidth, padHeight) =
                       Arith.correlationSize (Option.minimumOverlap opt) $
                       map (colorImageExtent . snd . picColored) pics
                    padExtent = Z :. padHeight :. padWidth
                in  (Just $ allOverlapsRun padExtent (Option.minimumOverlap opt),
                     optimalOverlap padExtent (Option.minimumOverlap opt))

   relations <-
      maybe (return Map.empty)
         (State.readDisplacement (map picPath pics))
         (Option.relations opt)

   let open = map ((\(mx,my) -> isNothing mx || isNothing my) . picParam) pics
   displacements <-
      forM (guardedPairs open $ zip [0..] pics) $
            \((ia, Picture pathA _ origA (leftTopA,picA)),
              (ib, Picture pathB _ origB (leftTopB,picB))) -> do
         forM_ maybeAllOverlapsShared $ \allOverlapsShared -> when False $
            writeGrey (Option.quality opt)
               (printf "/tmp/%s-%s-score.jpeg"
                  (FilePath.takeBaseName pathA) (FilePath.takeBaseName pathB)) $
               allOverlapsShared picA picB

         let relation = Map.lookup (pathA,pathB) relations
         md <-
            case (join $ fmap fst relation, join $ fmap snd relation) of
               (Just State.NonOverlapping, _) -> return Nothing
               (Just State.Overlapping, Just d) -> return $ Just d
               (related, _) -> do
                  let doffset@(dox,doy) = snd $ optimalOverlapShared picA picB
                  let diff = overlapDifferenceRun doffset picA picB
                  let overlapping =
                        related == Just State.Overlapping
                        ||
                        diff < Option.maximumDifference opt
                  let d = (fromIntegral dox + fst leftTopA - fst leftTopB,
                           fromIntegral doy + snd leftTopA - snd leftTopB)
                  info $
                     printf "%s - %s, %s, difference %f%s\n"
                        pathA pathB (show d) diff
                        (if overlapping then "" else " unrelated -> ignoring")
                  forM_ (Option.outputOverlap opt) $ \format ->
                     writeImage (Option.quality opt)
                        (printf format
                           (FilePath.takeBaseName pathA)
                           (FilePath.takeBaseName pathB)) $
                        composeOverlap doffset (origA, origB)
                  return $ toMaybe overlapping d
         return ((ia,ib), (pathA,pathB), md)

   forM_ (Option.outputState opt) $ \format ->
      State.writeDisplacement (printf format "relation") displacements

   let overlaps = mapMaybe (\(i,_paths,md) -> (,) i <$> md) displacements
   let (poss, dps) =
          absolutePositionsFromPairDisplacements
             (fixAtLeastOnePosition (0,0) $ map picParam pics) overlaps
   info "\nabsolute positions\n"
   info $ unlines $ map show poss

   info "\ncompare position differences with pair displacements\n"
   info $ unlines $
      zipWith
         (\(dpx,dpy) (dx,dy) ->
            printf "(%f,%f) (%f,%f)" dpx dpy dx dy)
         dps (map snd overlaps)
   let (errdx,errdy) =
          mapPair (maximum0, maximum0) $ unzip $
          zipWith
             (\(dpx,dpy) (dx,dy) -> (abs $ dpx - dx, abs $ dpy - dy))
             dps (map snd overlaps)

   info $
      "\n"
      ++
      printf "maximum horizontal error: %f\n" errdx
      ++
      printf "maximum vertical error: %f\n" errdy

   return (map picPath pics, map picColored pics, map (flip (,) 1) poss)


processOverlapRotate ::
   Option.Args ->
   IO ([FilePath], [(Degree Float, ColorImage8)],
       [((Float, Float), Complex Float)])
processOverlapRotate args = do
   let opt = Option.option args
   let info = CmdLine.info (Option.verbosity opt)
   let infoPlain = when (Option.verbosity opt >= Verbosity.verbose) . putStr

   pics <-
      map (mapPicParam (mapFst State.getAngleCorrection)) <$>
      processRotation args
   let padSize = Option.padSize opt
   let stampSize = Option.stampSize opt
   let optimalOverlapShared =
          optimalOverlapBigMulti
             (Z :. padSize :. padSize)
             (Z :. stampSize :. stampSize)
             (Option.numberStamps opt)
             (Option.minimumOverlap opt)

   relations <-
      maybe (return Map.empty)
         (State.readRotated (map picPath pics))
         (Option.relations opt)

   let open =
         map
            ((\(ma, (mx,my)) -> isNothing ma || isNothing mx || isNothing my)
             . picParam)
            pics
   displacements <-
      forM (guardedPairs open $ zip [0..] pics) $
            \((ia, Picture pathA _ _ (leftTopA,picA)),
              (ib, Picture pathB _ _ (leftTopB,picB))) -> do
         let relation = Map.lookup (pathA,pathB) relations
         correspondences <-
            case (join $ fmap fst relation, Fold.fold $ fmap snd relation) of
               (Just State.NonOverlapping, _) -> return []
               (Just State.Overlapping, corrs@(_:_)) -> return corrs
               (related, _) -> do
                  let add (x0,y0) (x1,y1) =
                        (fromIntegral x0 + x1, fromIntegral y0 + y1)
                  let mMaxDiff =
                        toMaybe (related /= Just State.Overlapping) $
                        Option.maximumDifference opt
                  let corrs =
                        map
                           (\(score,pa,pb) ->
                              (score, (add pa leftTopA, add pb leftTopB))) $
                        optimalOverlapShared mMaxDiff picA picB
                  info $ printf "left-top: %s, %s\n" (show leftTopA) (show leftTopB)
                  info $ printf "%s - %s\n" pathA pathB
                  forM_ corrs $ \(score, (pa@(xa,ya),pb@(xb,yb))) ->
                     info $
                        printf "%s ~ %s, (%f,%f), %f\n"
                           (show pa) (show pb) (xb-xa) (yb-ya) score
                  return $ map snd corrs
         return ((ia,ib), (pathA,pathB), correspondences)

   forM_ (Option.outputState opt) $ \format ->
      State.writeRotated (printf format "relation") displacements

   let overlaps = concatMap (\(i,_paths,ps) -> (,) i <$> ps) displacements
   let (posRots, dps) =
          layoutFromPairDisplacements
             (map (mapFst (fmap Degree.cis)) $
              fixAtLeastOneAnglePosition (Degree 0, (0,0)) $
              map picParam pics)
             overlaps
   info "\nabsolute positions and rotations: place, rotation (magnitude, phase)\n"
   infoPlain $ unlines $
      map
         (\((dx,dy),r) ->
            printf "(%8.2f,%8.2f), %8.6f :+ %9.6f (%8.6f, %7.3f)" dx dy
               (Complex.realPart r) (Complex.imagPart r)
               (Complex.magnitude r)
               (getDegree $ Degree.fromRadian $ Complex.phase r))
         posRots

   info "\ncompare position differences with pair displacements\n"
   infoPlain $ unlines $
      zipWith
         (\(dpx,dpy) (_i, ((xa,ya),(xb,yb))) ->
            printf "(%8.5f,%8.5f) (%7.2f,%7.2f) ~ (%7.2f,%7.2f)"
               dpx dpy xa ya xb yb)
         dps overlaps

   return (map picPath pics, map picColored pics, posRots)


processRotation ::
   (State.AngleCorrected angleCorr) =>
   Option.Args -> IO [Picture (angleCorr, (Maybe Float, Maybe Float))]
processRotation args = do
   let opt = Option.option args
   let notice = CmdLine.notice (Option.verbosity opt)
   let info = CmdLine.info (Option.verbosity opt)

   inputs <- Option.images args

   notice "\nfind rotation angles\n"
   picAngles <-
      forM inputs $ \(State.Proposed path (maybeAngle, _) _) -> do
         pic <- readImage (Option.verbosity opt) path
         let maxAngle = Option.maximumAbsoluteAngle opt
         let angles = Degree.linearScale (Option.numberAngleSteps opt) maxAngle
         when False $ analyseRotations angles pic
         when False $ fourierTransformation opt path pic
         angle <-
            case maybeAngle of
               Just angle -> return angle
               Nothing ->
                  if Option.radonTransform opt
                    then radonAngle (fmap negate maxAngle, maxAngle) pic
                    else return $ findOptimalRotation angles pic
         info $ printf "%s %f\176\n" path (getDegree angle)
         return (angle, pic)

   forM_ (Option.outputState opt) $ \format ->
      State.write (printf format "angle") $
         zipWith State.Angle (map State.propPath inputs) (map fst picAngles)

   notice "\nfind relative placements\n"
   let rotated = map (prepareOverlapMatching (Option.smooth opt)) picAngles

   when False $ do
      notice "write fft"
      let pic0 : pic1 : _ = map snd rotated
          size = (Z:.512:.1024 :: DIM2)
      writeGrey (Option.quality opt) "/tmp/padded.jpeg" $
         cudaRun1 (imageByteFromFloat . pad 0 (A.lift size)) pic0
      writeGrey (Option.quality opt) "/tmp/spectrum.jpeg" $
         cudaRun $ imageByteFromFloat $ A.map AComplex.magnitude $
         fft2DPlain CUFFT.forwardReal $
         cudaRun1 (pad 0 (A.lift size)) $
         pic0
      writeGrey (Option.quality opt) "/tmp/convolution.jpeg" $
         cudaRun $ imageByteFromFloat $ A.map (0.000001*) $
         correlatePadded size (A.use pic0) (A.use pic1)

   return $
      zipWith3
         (\(State.Proposed path (_,angleCorr) maybePos) colored plane ->
            Picture path (angleCorr, maybePos) colored plane)
         inputs picAngles rotated

process :: Option.Args -> IO ()
process args = do
   IO.hSetBuffering IO.stdout IO.LineBuffering
   IO.hSetBuffering IO.stderr IO.LineBuffering

   let opt = Option.option args
   let notice = CmdLine.notice (Option.verbosity opt)
   let info = CmdLine.info (Option.verbosity opt)

   (paths, picAngles, posRots) <-
      if Option.finetuneRotate opt
        then processOverlapRotate args
        else processOverlap args

   forM_ (Option.outputState opt) $ \format ->
      State.write (printf format "position") $
      zipWith3
         (\path (angle, _) (pos, rot) ->
            State.Position path
               (angle <> Degree.fromRadian (Complex.phase rot)) pos)
         paths picAngles posRots

   notice "\ncompose all parts\n"
   let ((canvasWidth, canvasHeight), rotMovPics, canvasMsgs) =
         Arith.canvasShape colorImageExtent
            (map (mapFst Degree.toRadian) picAngles) posRots
   mapM_ info canvasMsgs

   forM_ (Option.outputHard opt) $ \path ->
      writeImage (Option.quality opt) path $
      finalizeCountCanvas $
      foldl
         (flip updateCountCanvas)
         (emptyCountCanvas (Z :. 3 :. canvasHeight :. canvasWidth))
         rotMovPics

   notice "\ndistance maps\n"
   let geometryRelations =
         Arith.geometryRelations $
         map (Arith.geometryFeatures . mapThd3 colorImageExtent) rotMovPics

   forM_ (zip geometryRelations paths) $
         \((thisGeom, otherGeoms, allPoints), path) -> do

      let stem = FilePath.takeBaseName path
      let canvasShape = Z :. canvasHeight :. canvasWidth
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
     notice "\nweighted composition\n"
     writeImage (Option.quality opt) path $
      finalizeWeightedCanvas $
      foldl
         (\canvas ((thisGeom, otherGeoms, allPoints), (_rot, pic)) ->
            updateWeightedCanvas (Option.distanceGamma opt)
               thisGeom otherGeoms allPoints pic canvas)
         (emptyWeightedCanvas (Z :. 3 :. canvasHeight :. canvasWidth))
         (zip geometryRelations picAngles)

main :: IO ()
main = process =<< Option.get Option.Accelerate
