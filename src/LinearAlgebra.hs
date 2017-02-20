module LinearAlgebra where

import qualified Data.Packed.Matrix as Matrix
import qualified Data.Packed.Vector as Vector
import qualified Data.Packed.ST as PackST
import qualified Numeric.Container as Container
import Numeric.Container ((<\>), (<>))

import qualified Data.Complex as HComplex

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Control.Monad (zipWithM_)


-- we cannot use leastSquaresSelected here, because the right-hand side is not zero
absolutePositionsFromPairDisplacements ::
   Int -> [((Int, Int), (Float, Float))] ->
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
       pxs = matrix <\> Vector.fromList (map realToFrac dxs)
       pys = matrix <\> Vector.fromList (map realToFrac dys)
   in  (zip (0 : Vector.toList pxs) (0 : Vector.toList pys),
        zip (Vector.toList $ matrix <> pxs) (Vector.toList $ matrix <> pys))


leastSquaresSelected ::
   Matrix.Matrix Double -> [Maybe Double] ->
   ([Double], [Double])
leastSquaresSelected m mas =
   let (lhsCols,rhsCols) =
          ListHT.unzipEithers $
          zipWith
             (\col ma ->
                case ma of
                   Nothing -> Left col
                   Just a -> Right $ Container.scale a col)
             (Matrix.toColumns m) mas
       lhs = Matrix.fromColumns lhsCols
       rhs = foldl1 Container.add rhsCols
       sol = lhs <\> Container.scale (-1) rhs
   in  (snd $
        List.mapAccumL
           (curry $ \x ->
               case x of
                  (as, Just a) -> (as, a)
                  (a:as, Nothing) -> (as, a)
                  ([], Nothing) -> error "too few elements in solution vector")
           (Vector.toList sol) mas,
        Vector.toList $
        Container.add (lhs <> sol) rhs)

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

Maybe, dx and dy should be scaled down.
Otherwise they are weighted much more than the rotation.
-}
layoutFromPairDisplacements ::
   Int -> [((Int, (Float, Float)), (Int, (Float, Float)))] ->
   ([((Double,Double), HComplex.Complex Double)],
    [(Double,Double)])
layoutFromPairDisplacements numPics correspondences =
   let {-
       The weight will only influence the result
       for under-constrained equation systems.
       This is usually not the case.
       -}
       weight =
          let xs =
                 concatMap
                    (\((_ia,(xai,yai)),(_ib,(xbi,ybi))) -> [xai, yai, xbi, ybi])
                    correspondences
          in  realToFrac $ maximum xs - minimum xs
       matrix = PackST.runSTMatrix $ do
          mat <- PackST.newMatrix 0 (2 * length correspondences) (4*numPics)
          zipWithM_
             (\k ((ia,(xai,yai)),(ib,(xbi,ybi))) -> do
                let xa = realToFrac xai
                let xb = realToFrac xbi
                let ya = realToFrac yai
                let yb = realToFrac ybi
                PackST.writeMatrix mat (k+0) (4*ia+0) (-weight)
                PackST.writeMatrix mat (k+1) (4*ia+1) (-weight)
                PackST.writeMatrix mat (k+0) (4*ia+2) (-xa)
                PackST.writeMatrix mat (k+0) (4*ia+3) ya
                PackST.writeMatrix mat (k+1) (4*ia+2) (-ya)
                PackST.writeMatrix mat (k+1) (4*ia+3) (-xa)
                PackST.writeMatrix mat (k+0) (4*ib+0) weight
                PackST.writeMatrix mat (k+1) (4*ib+1) weight
                PackST.writeMatrix mat (k+0) (4*ib+2) xb
                PackST.writeMatrix mat (k+0) (4*ib+3) (-yb)
                PackST.writeMatrix mat (k+1) (4*ib+2) yb
                PackST.writeMatrix mat (k+1) (4*ib+3) xb)
             [0,2..] correspondences
          return mat
       {-
       We fix the first image to position (0,0) and rotation (1,0)
       in order to make the solution unique.
       -}
       (solution, projection) =
          leastSquaresSelected matrix
             (take (4*numPics) $
              map Just [0,0,1,0] ++ repeat Nothing)
   in  (map (\[dx,dy,rx,ry] -> ((weight*dx,weight*dy), rx HComplex.:+ ry)) $
        ListHT.sliceVertical 4 solution,
        map (\[x,y] -> (x,y)) $
        ListHT.sliceVertical 2 projection)
