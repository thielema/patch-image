module Main where

import qualified Codec.Picture.Png as PNG
import qualified Codec.Picture as Pic

import qualified Polygon
import Polygon (Polygon(PolygonCW))
import Point2 (Point2(Point2))


triangle :: Pic.Image Pic.PixelRGB8
triangle =
   let poly = PolygonCW [Point2 (10, 10), Point2 (200, 100), Point2 (100, 200)]
       render xi yi =
          let x, y :: Integer
              x = fromIntegral xi
              y = fromIntegral yi
          in  if Polygon.contains poly $ Point2 (x, y)
                then Pic.PixelRGB8 0 0 255
                else Pic.PixelRGB8 255 255 255
   in  Pic.generateImage render 256 256


gradient :: Pic.Image Pic.PixelRGB8
gradient =
   Pic.generateImage
      (\x y -> Pic.PixelRGB8 (fromIntegral x) (fromIntegral y) 128)
      256 256


main :: IO ()
main = do
   PNG.writePng "/tmp/triangle.png" triangle
   PNG.writePng "/tmp/test.png" gradient
