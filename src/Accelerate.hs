{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.IO as AIO
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Acc, Array, Exp, DIM3, (:.)((:.)), Z(Z), )

import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV

import Text.Printf (printf)

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
                 xc = max 0 $ min (width -1) $ fastRound xsrc
                 yc = max 0 $ min (height-1) $ fastRound ysrc
             in  arr A.! A.lift (Z :. yc :. xc :. chan)


main :: IO ()
main = do
   writeImage 90 "/tmp/test.jpeg" .
      CUDA.run1
         (imageByteFromFloat . floatArray .
          rotate (cos 1, sin 1) . imageFloatFromByte)
      =<< readImage "data/mpa0.jpeg"
