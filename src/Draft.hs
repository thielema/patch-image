module Main where

import qualified Codec.Picture.Png as PNG
import qualified Codec.Picture as Pic

import qualified Polygon
import qualified Line
import Polygon (Polygon(PolygonCW))
import Line (Line2, Line(Segment))
import Point2 (Point2(Point2), distance)

import qualified Data.List.Match as Match
import Control.Monad (liftM2)
import Data.List.HT (removeEach)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (catMaybes, mapMaybe)


project ::
   (Fractional a, Ord a) =>
   Point2 a -> (Point2 a, Point2 a) -> Maybe (Point2 a)
project x (a, b) =
   let (r, _, _, y) = Line.distanceAux a b x
   in  toMaybe (0<=r && r<=1) y

segments :: (Eq a, Num a) => [Point2 a] -> [Line2 a]
segments = map (uncurry Segment) . Polygon.edges

intersections ::
   (Fractional a, Ord a) =>
   [Line2 a] -> [Line2 a] -> [Point2 a]
intersections segments0 segments1 =
   catMaybes $ liftM2 Line.intersect segments0 segments1

averageRGB :: (Eq a, Fractional a) => [(a, (a,a,a))] -> (a,a,a)
averageRGB weightColors =
   let (rs0, cs) = unzip weightColors
       rs = if all (0==) rs0 then Match.replicate rs0 1 else rs0
       rsum = sum rs
       scale r (red,green,blue) = (r*red, r*green, r*blue)
       add (red0,green0,blue0) (red1,green1,blue1) =
          (red0+red1, green0+green1, blue0+blue1)
   in  foldl1 add $ zipWith (\r c -> scale (r/rsum) c) rs cs

polyMany ::
   (Floating a, RealFrac a) =>
   [((a,a,a), [Point2 a])] -> Pic.Image Pic.PixelRGB8
polyMany colorPointss =
   let pointss = map snd colorPointss
       polys = map PolygonCW pointss
       intPointss =
          map
             (\(points, otherPointss) ->
                intersections (segments points)
                   (concatMap segments otherPointss)) $
          removeEach pointss
       render xi yi =
          let x = fromIntegral xi
              y = fromIntegral yi
              p = Point2 (x,y)
              minDist intPoints others this =
                 minimum $ map (distance p) $
                 (intPoints ++) $
                 filter (\q -> any (flip Polygon.contains q) others) $
                 (this++) $ mapMaybe (project p) $ Polygon.edges this
              minDists =
                 zipWith
                    (\intPoints (this, others) ->
                       minDist intPoints (map PolygonCW others) this)
                    intPointss (removeEach pointss)
              contained = map (flip Polygon.contains p) polys
              weightColors =
                 map snd $
                 filter fst $
                 zip contained $
                 zip minDists (map fst colorPointss)
              (red,green,blue) =
                 case weightColors of
                    [] -> (1,1,1)
                    [(_, color)] -> color
                    _ -> averageRGB weightColors
          in  Pic.PixelRGB8
                 (round (red*255)) (round (green*255)) (round (blue*255))
   in  Pic.generateImage render 256 256

poly2 ::
   (Floating a, RealFrac a) =>
   [Point2 a] -> [Point2 a] -> Pic.Image Pic.PixelRGB8
poly2 points0 points1 =
   let poly0 = PolygonCW points0
       poly1 = PolygonCW points1
       intPoints = intersections (segments points0) (segments points1)
       render xi yi =
          let x = fromIntegral xi
              y = fromIntegral yi
              p = Point2 (x,y)
              minDist other this =
                 minimum $ map (distance p) $
                 (intPoints ++) $ filter (Polygon.contains other) $
                 (this++) $ mapMaybe (project p) $ Polygon.edges this
              minDist0 = minDist poly1 points0
              minDist1 = minDist poly0 points1
              scale = 255 / (minDist0 + minDist1)
          in  case (Polygon.contains poly0 p, Polygon.contains poly1 p) of
                 (False, False) -> Pic.PixelRGB8 255 255 255
                 (False, True) -> Pic.PixelRGB8 0 0 255
                 (True, False) -> Pic.PixelRGB8 255 0 0
                 (True, True) ->
                    Pic.PixelRGB8
                       (round (minDist0*scale)) 0
                       (round (minDist1*scale))
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


triA, triB :: [Point2 Double]
triA = [Point2 (10, 10), Point2 (245, 100), Point2 (100, 245)]
triB = map (\(Point2 (x,y)) -> Point2 (255-x,y)) triA

isoBox :: (Eq a, Num a) => (a, a) -> (a, a) -> [Point2 a]
isoBox (l,t) (r,b) =
   [Point2 (l,t), Point2 (r,t), Point2 (r,b), Point2 (l,b)]

boxA, boxB :: [Point2 Double]
boxA = isoBox (10, 10) (200, 200)
boxB = isoBox (55, 55) (245, 245)

buxA, buxB, buxC, buxD :: [Point2 Double]
buxA = isoBox ( 10, 10) (150, 100)
buxB = isoBox (110, 10) (250, 100)
buxC = isoBox ( 30, 50) (170, 150)
buxD = isoBox (110, 80) (249, 170)

main :: IO ()
main = do
   PNG.writePng "/tmp/mixed.png" $
      polyMany [((1,0,0), boxA), ((1,1,0), boxB),
                ((0,1,0), triA), ((0,0,1), triB)]
   PNG.writePng "/tmp/boxes.png" $
      polyMany [((1,0,0), buxA), ((1,1,0), buxB),
                ((0,1,0), buxC), ((0,0,1), buxD)]
   PNG.writePng "/tmp/box2.png" $ poly2 boxA boxB
   PNG.writePng "/tmp/triangle2.png" $ poly2 triA triB
   PNG.writePng "/tmp/triangle.png" triangle
   PNG.writePng "/tmp/test.png" gradient
