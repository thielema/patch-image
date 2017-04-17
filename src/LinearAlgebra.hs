module LinearAlgebra where

import qualified Data.Packed.Matrix as Matrix
import qualified Data.Packed.Vector as Vector
import qualified Data.Packed.ST as PackST
import qualified Numeric.Container as Container
import Numeric.Container ((<\>), (<>))

import Data.Complex (Complex((:+)))

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (mapPair, mapSnd)
import Data.Maybe (isJust, fromMaybe)

import Control.Applicative ((<$>))


fixAtLeastOne :: a -> [Maybe a] -> [Maybe a]
fixAtLeastOne zero ms =
   case (any isJust ms, ms) of
      (True, _) -> ms
      (False, _:nothings) -> Just zero : nothings
      (False, []) -> error "fixAtLeastOne: empty image list"

{- |
If no coordinate is fixed, then the first one will be fixed to the given value.
This is not strictly necessary.
Without a fixed coordinate,
the solver will center all solutions around zero.
However, there will not necessarily be an image with a zero coordinate,
which is somehow ugly.
-}
fixAtLeastOnePosition ::
   (a,b) -> [(Maybe a, Maybe b)] -> [(Maybe a, Maybe b)]
fixAtLeastOnePosition (a,b) =
   parallel (fixAtLeastOne a, fixAtLeastOne b)

fixAtLeastOneAnglePosition ::
   (angle, (a,b)) ->
   [(Maybe angle, (Maybe a, Maybe b))] ->
   [(Maybe angle, (Maybe a, Maybe b))]
fixAtLeastOneAnglePosition (angle, ab) =
   parallel (fixAtLeastOne angle, fixAtLeastOnePosition ab)

parallel :: ([a0] -> [a1], [b0] -> [b1]) -> ([(a0,b0)] -> [(a1,b1)])
parallel fs = uncurry zip . mapPair fs . unzip


sparseMatrix :: Int -> Int -> [((Int, Int), Double)] -> Matrix.Matrix Double
sparseMatrix numRows numCols xs =
   PackST.runSTMatrix $ do
      mat <- PackST.newMatrix 0 numRows numCols
      mapM_ (\((r,c), x) -> PackST.writeMatrix mat r c x) xs
      return mat

elm :: Int -> Int -> a -> ((Int, Int), a)
elm row col x = ((row, col), x)


absolutePositionsFromPairDisplacements ::
   [(Maybe Float, Maybe Float)] -> [((Int, Int), (Float, Float))] ->
   ([(Double,Double)], [(Double,Double)])
absolutePositionsFromPairDisplacements mxys displacements =
   let numPics = length mxys
       (mxs, mys) = unzip mxys
       (is, (dxs, dys)) = mapSnd unzip $ unzip displacements
       matrix =
          sparseMatrix (length is) numPics $ concat $
          zipWith (\k (ia,ib) -> [elm k ia (-1), elm k ib 1]) [0..] is
       solve ms ds =
          leastSquaresSelected matrix
             (map (fmap realToFrac) ms)
             (Vector.fromList (map realToFrac ds))
       (pxs, achievedDxs) = solve mxs dxs
       (pys, achievedDys) = solve mys dys
   in  (zip pxs pys, zip achievedDxs achievedDys)


leastSquaresSelected ::
   Matrix.Matrix Double -> [Maybe Double] -> Vector.Vector Double ->
   ([Double], [Double])
leastSquaresSelected m mas rhs0 =
   let (lhsCols,rhsCols) =
          ListHT.unzipEithers $
          zipWith
             (\col ma ->
                case ma of
                   Nothing -> Left col
                   Just a -> Right $ Container.scale a col)
             (Matrix.toColumns m) mas
       lhs = Matrix.fromColumns lhsCols
       rhs = foldl Container.add (Container.scale 0 rhs0) rhsCols
       sol = lhs <\> Container.sub rhs0 rhs
   in  if Vector.dim rhs0 == 0 then (map (fromMaybe 0) mas, []) else
       (snd $
        List.mapAccumL
           (curry $ \x ->
               case x of
                  (as, Just a) -> (as, a)
                  (a:as, Nothing) -> (as, a)
                  ([], Nothing) -> error "too few elements in solution vector")
           (Vector.toList sol) mas,
        Vector.toList $
        Container.add (lhs <> sol) rhs)


zeroVector, _zeroVector :: Int -> Vector.Vector Double
zeroVector n = Vector.fromList $ replicate n 0
-- fails for vectors of size 0
_zeroVector n = Container.constant 0 n

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

We try to scale dx and dy down using 'weight'.
Otherwise they are weighted much more than the rotation.
However the weight will only influence the result
for under-constrained equation systems.
This is usually not the case.
-}
layoutFromPairDisplacements ::
   [(Maybe (Float, Float), (Maybe Float, Maybe Float))] ->
   [((Int, Int), ((Float, Float), (Float, Float)))] ->
   ([((Double,Double), Complex Double)],
    [(Double,Double)])
layoutFromPairDisplacements mrxys correspondences =
   let numPics = length mrxys
       weight =
          let xs =
                 concatMap
                    (\(_i, ((xai,yai),(xbi,ybi))) -> [xai, yai, xbi, ybi])
                    correspondences
          in  if null xs then 1 else realToFrac $ maximum xs - minimum xs
       matrix =
          sparseMatrix (2 * length correspondences) (4*numPics) $ concat $
          zipWith
             (\k ((ia,ib), ((xai,yai),(xbi,ybi))) ->
                let xa = realToFrac xai
                    xb = realToFrac xbi
                    ya = realToFrac yai
                    yb = realToFrac ybi
                in  elm (k+0) (4*ia+0) (-weight) :
                    elm (k+1) (4*ia+1) (-weight) :
                    elm (k+0) (4*ia+2) (-xa) :
                    elm (k+0) (4*ia+3) ya :
                    elm (k+1) (4*ia+2) (-ya) :
                    elm (k+1) (4*ia+3) (-xa) :
                    elm (k+0) (4*ib+0) weight :
                    elm (k+1) (4*ib+1) weight :
                    elm (k+0) (4*ib+2) xb :
                    elm (k+0) (4*ib+3) (-yb) :
                    elm (k+1) (4*ib+2) yb :
                    elm (k+1) (4*ib+3) xb :
                    [])
             [0,2..] correspondences
       (solution, projection) =
          leastSquaresSelected matrix
             (concatMap
                (\(mr, (mx,my)) ->
                   [(/weight) . realToFrac <$> mx,
                    (/weight) . realToFrac <$> my,
                    realToFrac . fst <$> mr,
                    realToFrac . snd <$> mr]) $
              mrxys)
             (zeroVector (2 * length correspondences))
   in  (map (\[dx,dy,rx,ry] -> ((weight*dx,weight*dy), rx :+ ry)) $
        ListHT.sliceVertical 4 solution,
        map (\[x,y] -> (x,y)) $
        ListHT.sliceVertical 2 projection)
