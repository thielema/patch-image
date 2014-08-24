module Main where

import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as AIO
import Data.Array.Accelerate (Acc, Array, DIM3, (:.)((:.)), Z(Z), )

import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV

import Text.Printf (printf)

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
imageByteFromFloat = A.map (A.floor . (0.5+) . (255*) . max 0 . min 1)

floatArray :: Acc (Array sh Float) -> Acc (Array sh Float)
floatArray = id


main :: IO ()
main = do
   writeImage 90 "/tmp/test.jpeg" .
      CUDA.run1 (imageByteFromFloat . floatArray . imageFloatFromByte)
      =<< readImage "data/mpa0.jpeg"
