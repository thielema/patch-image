module LinearAlgebra where

import qualified Matrix.Sparse as Sparse
import qualified Matrix.Vector as Vector
import qualified Matrix.QR.Givens as QR

import Data.Complex (Complex((:+)))

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Map as Map
import Data.Array (Array)
import Data.Tuple.HT (mapPair, mapSnd)
import Data.Maybe (isJust)
import Data.Map (Map)

import Control.Applicative ((<$>), (<$))


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


type Matrix = Sparse.Matrix Int Int
type Vector = Array Int

sparseMatrix :: Int -> Int -> [((Int, Int), Float)] -> Matrix Float
sparseMatrix numRows numCols =
   Sparse.fromMap ((0,0), (numRows-1, numCols-1)) . Map.fromList

-- every column must be non-empty
sparseToColumns :: Matrix a -> [Map Int a]
sparseToColumns = Map.elems . Sparse.toColumns

sparseFromColumns :: (Int,Int) -> [Map Int a] -> Matrix a
sparseFromColumns (m0,m1) cols =
   Sparse.fromColumns ((m0,0), (m1, length cols-1)) $
   Map.fromList $ zip [0..] cols

addSparseColumn :: (Num a) => Vector a -> Map Int a -> Vector a
addSparseColumn v col = Array.accum (+) v $ Map.toList col


elm :: Int -> Int -> a -> ((Int, Int), a)
elm row col x = ((row, col), x)


absolutePositionsFromPairDisplacements ::
   [(Maybe Float, Maybe Float)] -> [((Int, Int), (Float, Float))] ->
   ([(Float,Float)], [(Float,Float)])
absolutePositionsFromPairDisplacements mxys displacements =
   let numPics = length mxys
       (mxs, mys) = unzip mxys
       (is, (dxs, dys)) = mapSnd unzip $ unzip displacements
       matrix =
          sparseMatrix (length is) numPics $ concat $
          zipWith (\k (ia,ib) -> [elm k ia (-1), elm k ib 1]) [0..] is
       solve ms ds = leastSquaresSelected matrix ms (Vector.fromList ds)
       (pxs, achievedDxs) = solve mxs dxs
       (pys, achievedDys) = solve mys dys
   in  (zip pxs pys, zip achievedDxs achievedDys)


leastSquaresSelected ::
   Matrix Float -> [Maybe Float] -> Vector Float -> ([Float], [Float])
leastSquaresSelected m mas rhs0 =
   let (lhsCols,rhsCols) =
          ListHT.unzipEithers $
          zipWith
             (\col ma ->
                case ma of
                   Nothing -> Left col
                   Just a -> Right $ fmap (a*) col)
             (sparseToColumns m) mas
       lhs = sparseFromColumns (Array.bounds rhs0) lhsCols
       rhs = foldl addSparseColumn (0 <$ rhs0) rhsCols
       sol = QR.leastSquares lhs $ Vector.sub rhs0 rhs
   in  (snd $
        List.mapAccumL
           (curry $ \x ->
               case x of
                  (as, Just a) -> (as, a)
                  (a:as, Nothing) -> (as, a)
                  ([], Nothing) -> error "too few elements in solution vector")
           (Vector.toList sol) mas,
        Vector.toList $ Vector.add (Sparse.mulVector lhs sol) rhs)


zeroVector :: Int -> Vector Float
zeroVector n = Vector.fromList $ replicate n 0

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

Formerly, we tried to scale dx and dy down using a weight,
such that dx and dy had magnitude similar to c and s.
However the weight would only influence the result
for under-constrained equation systems.
This is usually not the case and
the solver does not support rank-deficient matrices, anyway.
-}
layoutFromPairDisplacements ::
   [(Maybe (Float, Float), (Maybe Float, Maybe Float))] ->
   [((Int, Int), ((Float, Float), (Float, Float)))] ->
   ([((Float,Float), Complex Float)], [(Float,Float)])
layoutFromPairDisplacements mrxys correspondences =
   let numPics = length mrxys
       matrix =
          sparseMatrix (2 * length correspondences) (4*numPics) $ concat $
          zipWith
             (\k ((ia,ib), ((xa,ya),(xb,yb))) ->
                elm (k+0) (4*ia+0) (-1) :
                elm (k+1) (4*ia+1) (-1) :
                elm (k+0) (4*ia+2) (-xa) :
                elm (k+0) (4*ia+3) ya :
                elm (k+1) (4*ia+2) (-ya) :
                elm (k+1) (4*ia+3) (-xa) :
                elm (k+0) (4*ib+0) 1 :
                elm (k+1) (4*ib+1) 1 :
                elm (k+0) (4*ib+2) xb :
                elm (k+0) (4*ib+3) (-yb) :
                elm (k+1) (4*ib+2) yb :
                elm (k+1) (4*ib+3) xb :
                [])
             [0,2..] correspondences
       (solution, projection) =
          leastSquaresSelected matrix
             (concatMap (\(mr, (mx,my)) -> [mx, my, fst <$> mr, snd <$> mr]) $
              mrxys)
             (zeroVector (2 * length correspondences))
   in  (map (\[dx,dy,rx,ry] -> ((dx,dy), rx :+ ry)) $
        ListHT.sliceVertical 4 solution,
        map (\[x,y] -> (x,y)) $
        ListHT.sliceVertical 2 projection)
