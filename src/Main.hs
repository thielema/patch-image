module Main where

import qualified Codec.Picture.Png as PNG
import qualified Codec.Picture as Pic

import qualified Polygon
import qualified Line
import SegmentIntersection (naiveIntersections)
import Polygon (Polygon(PolygonCW))
import Line (Line(Segment))
import Point2 (Point2(Point2), distance)

import Data.Maybe.HT (toMaybe)
import Data.Maybe (mapMaybe)


project ::
   (Fractional a, Ord a) =>
   Point2 a -> (Point2 a, Point2 a) -> Maybe (Point2 a)
project x (a, b) =
   let (r, _, _, y) = Line.distanceAux a b x
   in  toMaybe (0<=r && r<=1) y

triangle2 :: Pic.Image Pic.PixelRGB8
triangle2 =
   let points0 = [Point2 (10, 10), Point2 (245, 100), Point2 (100, 245)]
       points1 = map (\(Point2 (x,y)) -> Point2 (255-x,y)) points0
       poly0 = PolygonCW points0
       poly1 = PolygonCW points1
       segments = map (uncurry Segment) . Polygon.edges
       intPoints = naiveIntersections $ segments points0 ++ segments points1
       render xi yi =
          let x, y :: Double
              x = fromIntegral xi
              y = fromIntegral yi
              p = Point2 (x,y)
              minDist0 =
                 minimum $ map (distance p) $
                 (intPoints ++) $
                 filter (Polygon.contains poly1) $
                 (points0++) $
                 mapMaybe (project p) $ Polygon.edges points0
              minDist1 =
                 minimum $ map (distance p) $
                 (intPoints ++) $
                 filter (Polygon.contains poly0) $
                 (points1++) $
                 mapMaybe (project p) $ Polygon.edges points1
              minDistSum = minDist0 + minDist1
          in  case (Polygon.contains poly0 p, Polygon.contains poly1 p) of
                 (False, False) -> Pic.PixelRGB8 255 255 255
                 (False, True) -> Pic.PixelRGB8 0 0 255
                 (True, False) -> Pic.PixelRGB8 255 0 0
                 (True, True) ->
                    Pic.PixelRGB8
                       (round (minDist0/minDistSum*255)) 0
                       (round (minDist1/minDistSum*255))
   in  Pic.generateImage render 256 256


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
   PNG.writePng "/tmp/triangle2.png" triangle2
   PNG.writePng "/tmp/triangle.png" triangle
   PNG.writePng "/tmp/test.png" gradient
