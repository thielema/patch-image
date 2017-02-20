module Arithmetic where

import qualified Data.Complex as Complex
import Data.Complex (Complex, )

import Control.Monad (liftM2, guard)

import qualified Data.Bits as Bit
import Data.Maybe (catMaybes)
import Data.Tuple.HT (mapPair)


inBox ::
   (Ord a, Num a) =>
   (a, a) ->
   (a, a) ->
   Bool
inBox (width,height) (x,y) =
   0<=x && x<width && 0<=y && y<height


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
boundingBoxOfRotated = boundingBoxOfRotatedGen (min,max)

boundingBoxOfRotatedGen ::
   (Num a) => (a -> a -> a, a -> a -> a) -> (a,a) -> (a,a) -> ((a,a), (a,a))
boundingBoxOfRotatedGen (mini,maxi) rot (w,h) =
   let (xs,ys) =
          unzip $
          rotatePoint rot (0,0) :
          rotatePoint rot (w,0) :
          rotatePoint rot (0,h) :
          rotatePoint rot (w,h) :
          []
   in  ((foldl1 mini xs, foldl1 maxi xs), (foldl1 mini ys, foldl1 maxi ys))


linearIp :: (Num a) => (a,a) -> a -> a
linearIp (x0,x1) t = (1-t) * x0 + t * x1

cubicIp :: (Fractional a) => (a,a,a,a) -> a -> a
cubicIp (xm1, x0, x1, x2) t =
   let lipm12 = linearIp (xm1,x2) t
       lip01  = linearIp (x0, x1) t
   in  lip01 + (t*(t-1)/2) * (lipm12 + (x0+x1) - 3 * lip01)


data Vec a v =
   Vec {vecZero :: v, vecAdd :: v -> v -> v, vecScale :: a -> v -> v}

vecScalar :: (Num a) => Vec a a
vecScalar = Vec 0 (+) (*)


linearIpVec :: (Num a) => Vec a v -> (v,v) -> a -> v
linearIpVec vec (x0,x1) t =
   vecAdd vec (vecScale vec (1-t) x0) (vecScale vec t x1)

cubicIpVec :: (Fractional a) => Vec a v -> (v,v,v,v) -> a -> v
cubicIpVec vec (xm1, x0, x1, x2) t =
   let lipm12 = linearIpVec vec (xm1,x2) t
       lip01  = linearIpVec vec (x0, x1) t
   in  vecAdd vec lip01 $
       vecScale vec (t*(t-1)/2)
         (foldl1 (vecAdd vec) [lipm12, x0, x1, vecScale vec (-3) lip01])



smooth3 :: (Fractional a) => (a,a,a) -> a
smooth3 (l,m,r) = (l+2*m+r)/4



type Point2 a = (a,a)

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


projectPerp ::
   (Fractional a) =>
   Point2 a -> (Point2 a, Point2 a) -> (a, Point2 a)
projectPerp (xc,yc) ((xa,ya), (xb,yb)) =
   let dx = xb-xa
       dy = yb-ya
       r = ((xc-xa)*dx + (yc-ya)*dy) / (dx*dx + dy*dy)
   in  (r, (xa + r*dx, ya + r*dy))


distanceSqr :: (Num a) => Point2 a -> Point2 a -> a
distanceSqr (xa,ya) (xb,yb) = (xa-xb)^(2::Int) + (ya-yb)^(2::Int)

distance :: (Floating a) => Point2 a -> Point2 a -> a
distance a b = sqrt $ distanceSqr a b



ceilingPow2 :: Int -> Int
ceilingPow2 n =
   Bit.setBit 0 $ ceiling $ logBase 2 (fromIntegral n :: Double)

-- cf. numeric-prelude
divUp :: (Integral a) => a -> a -> a
divUp a b = - div (-a) b



pairFromComplex :: (RealFloat a) => Complex a -> (a,a)
pairFromComplex z = (Complex.realPart z, Complex.imagPart z)

mapComplex :: (a -> b) -> Complex a -> Complex b
mapComplex f (r Complex.:+ i)  =  f r Complex.:+ f i
