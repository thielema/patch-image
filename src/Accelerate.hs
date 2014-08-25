{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.Array.Accelerate.Math.FFT as FFT
import qualified Data.Array.Accelerate.Math.Complex as Complex
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.IO as AIO
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Math.Complex (Complex, )
import Data.Array.Accelerate
          (Acc, Array, Exp, DIM1, DIM2, DIM3, (:.)((:.)), Z(Z), Any(Any),
           (<*), (&&*), (?), )

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

import Text.Printf (printf)

import qualified Data.List.Key as Key
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

imageFloatFromByte ::
   (A.Shape sh, A.Elt a, A.IsFloating a) =>
   Acc (Array sh Word8) -> Acc (Array sh a)
imageFloatFromByte = A.map ((/255) . A.fromIntegral)

imageByteFromFloat ::
   (A.Shape sh, A.Elt a, A.IsFloating a) =>
   Acc (Array sh a) -> Acc (Array sh Word8)
imageByteFromFloat = A.map (fastRound . (255*) . max 0 . min 1)

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

indexLimit ::
   (A.Elt a) => Acc (Array DIM3 a) -> (Exp Int, Exp Int, Exp Int) -> Exp a
indexLimit arr (x,y,c) =
   let (Z :. height :. width :. _chans) =
          A.unlift $ A.shape arr :: ExpDIM3
       xc = max 0 $ min (width -1) x
       yc = max 0 $ min (height-1) y
   in  arr A.! A.lift (Z :. yc :. xc :. c)

indexFrac ::
   (A.Elt a, A.IsFloating a) =>
   Acc (Array DIM3 a) -> (Exp a, Exp a, Exp Int) -> Exp a
indexFrac arr (x,y,c) =
   let (xi,xf) = splitFraction x
       (yi,yf) = splitFraction y
       interpolRow yc =
          cubicIp
             (indexLimit arr (xi-1,yc,c),
              indexLimit arr (xi,  yc,c),
              indexLimit arr (xi+1,yc,c),
              indexLimit arr (xi+2,yc,c))
             xf
   in  cubicIp
          (interpolRow (yi-1),
           interpolRow  yi,
           interpolRow (yi+1),
           interpolRow (yi+2))
          yf


type ExpDIM2 = Z :. Exp Int :. Exp Int
type ExpDIM3 = Z :. Exp Int :. Exp Int :. Exp Int

rotate ::
   (A.Elt a, A.IsFloating a) =>
   (Exp a, Exp a) ->
   Acc (Array DIM3 a) -> Acc (Array DIM3 a)
rotate rot arr =
   let (Z :. height :. width :. chans) =
          A.unlift $ A.shape arr :: ExpDIM3
       ((left, right), (top, bottom)) =
          boundingBoxOfRotated rot (A.fromIntegral width, A.fromIntegral height)
   in  A.generate
          (A.lift (Z :. A.ceiling (bottom-top) :. A.ceiling (right-left) :. chans)) $ \p ->
             let (Z :. ydst :. xdst :. chan) = A.unlift p :: ExpDIM3
                 (xsrc,ysrc) =
                    rotatePoint (mapSnd negate rot)
                       (A.fromIntegral xdst + left,
                        A.fromIntegral ydst + top)
             in  indexFrac arr (xsrc, ysrc, chan)


brightnessPlane :: Acc (Array DIM3 Float) -> Acc (Array DIM2 Float)
brightnessPlane = flip A.slice (A.lift (Any :. (0::Int)))

brightnessPlaneRun :: Array DIM3 Float -> Array DIM2 Float
brightnessPlaneRun = CUDA.run1 brightnessPlane

rowHistogram :: Acc (Array DIM3 Float) -> Acc (Array DIM1 Float)
rowHistogram = A.fold (+) 0 . brightnessPlane


unliftRotationParameters ::
   Acc ((A.Scalar Float, A.Scalar Float), Array DIM3 Word8) ->
   ((Acc (A.Scalar Float), Acc (A.Scalar Float)), Acc (Array DIM3 Word8))
unliftRotationParameters arg =
   let (cs, arr) =
          A.unlift arg
             :: (Acc (A.Scalar Float, A.Scalar Float),
                 Acc (Array DIM3 Word8))
   in  (A.unlift cs, arr)

rotateHistogram ::
   Float -> Array DIM3 Word8 -> (Array DIM3 Word8, Array DIM1 Float)
rotateHistogram =
   let rot =
          CUDA.run1 $ \arg ->
             let ((c,s), arr) = unliftRotationParameters arg
                 rotated =
                    rotate (A.the c, A.the s) $ imageFloatFromByte arr
             in  A.lift
                    (imageByteFromFloat rotated,
                     rowHistogram rotated)
   in  \angle arr ->
          rot ((A.fromList Z [cos angle], A.fromList Z [sin angle]), arr)


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
             let ((c,s), arr) = unliftRotationParameters arg
             in  A.sum $ A.map (^(2::Int)) $ differentiate $ rowHistogram $
                 rotate (A.the c, A.the s) $ imageFloatFromByte arr
   in  \angle arr ->
          A.indexArray
             (rot ((A.fromList Z [cos angle], A.fromList Z [sin angle]), arr)) Z

findOptimalRotation :: Array DIM3 Word8 -> Float
findOptimalRotation pic =
   Key.maximum (flip scoreRotation pic . (* (pi/180))) $
   map (\a -> fromIntegral a / 100) [-100,-95..100::Int]





rotateManifest :: Float -> Array DIM3 Word8 -> Array DIM3 Float
rotateManifest =
   let rot =
          CUDA.run1 $ \arg ->
             let ((c,s), arr) = unliftRotationParameters arg
             in  rotate (A.the c, A.the s) $ imageFloatFromByte arr
   in  \angle arr ->
          rot ((A.fromList Z [cos angle], A.fromList Z [sin angle]), arr)


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

padToComplex ::
   (A.Elt a, A.IsNum a) =>
   (A.Scalar DIM2, Array DIM2 a) -> Array DIM2 (Complex a)
padToComplex =
   CUDA.run1 $ \shz ->
      let (sh,z) = A.unlift shz
      in  A.map (A.lift . (Complex.:+ 0)) $ pad (A.the sh) z

convolvePadded ::
   (A.Elt a, A.IsFloating a) =>
   DIM2 -> Array DIM2 a -> Array DIM2 a -> Acc (Array DIM2 a)
convolvePadded sh x y =
   let forward z =
          FFT.fft2D FFT.Forward $
          padToComplex (A.fromList Z [sh], z)
   in  A.map Complex.real $
       FFT.fft2D FFT.Inverse $ CUDA.run $
       A.zipWith (\xi yi -> xi * Complex.conj yi) (forward x) (forward y)


argmaximum ::
   (A.Elt a, A.IsScalar a) =>
   Exp Int -> Exp Int ->
   Acc (Array DIM2 a) -> Acc (A.Scalar ((Int, Int), a))
argmaximum xsplit ysplit arr =
   let (Z :. height :. width) = A.unlift $ A.shape arr :: ExpDIM2
       decorated =
          A.generate (A.shape arr) $ \p ->
             let (Z:.y:.x) = A.unlift p :: ExpDIM2
                 wrap size split c = c<*split ? (c, c-size)
             in  A.lift ((wrap height ysplit y, wrap width xsplit x), arr A.! p)
       argmax x y  =  A.snd x <* A.snd y ? (y,x)
   in  A.fold1All argmax decorated


optimalOverlap :: Array DIM2 Float -> Array DIM2 Float -> ((Int, Int), Float)
optimalOverlap a b =
   let (Z :. heighta :. widtha) = A.arrayShape a
       (Z :. heightb :. widthb) = A.arrayShape b
       width  = ceilingPow2 $ widtha  + widthb
       height = ceilingPow2 $ heighta + heightb
       sh = Z :. height :. width
       half = flip div 2
   in  A.indexArray
          (CUDA.run $
           argmaximum
              (A.lift $ half $ width-widthb+widtha)
              (A.lift $ half $ height-heightb+heighta) $
           convolvePadded sh a b) Z

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


main :: IO ()
main = do
   paths <- Env.getArgs
   putStrLn "\nfind rotation angles"
   rotated <-
      forM paths $ \path -> do
         pic <- readImage path
         when False $ analyseRotations pic
         let angle = findOptimalRotation pic
         printf "%s %f\n" path angle
         return (path, rotateManifest (angle*pi/180) pic)

   putStrLn "\nfind relative placements"
   let pairs = do
          (a:as) <- tails $ zip [0..] $ map (mapSnd brightnessPlaneRun) rotated
          b <- as
          return (a,b)
   displacements <-
      forM pairs $ \((ia,(pathA,picA)), (ib,(pathB,picB))) -> do
         let ((dy,dx), score) = optimalOverlap picA picB
         printf "%s - %s, %s %f\n" pathA pathB (show (dx,dy)) score
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
