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
import Data.Maybe (isJust)

import Control.Monad (zipWithM_)
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
fixAtLeastOneDisplacement ::
   (a,b) -> [(Maybe a, Maybe b)] -> [(Maybe a, Maybe b)]
fixAtLeastOneDisplacement (a,b) =
   uncurry zip . mapPair (fixAtLeastOne a, fixAtLeastOne b) . unzip

absolutePositionsFromPairDisplacements ::
   [(Maybe Float, Maybe Float)] -> [((Int, Int), (Float, Float))] ->
   ([(Double,Double)], [(Double,Double)])
absolutePositionsFromPairDisplacements mxys displacements =
   let numPics = length mxys
       (mxs, mys) = unzip mxys
       (is, (dxs, dys)) = mapSnd unzip $ unzip displacements
       matrix = PackST.runSTMatrix $ do
          mat <- PackST.newMatrix 0 (length is) numPics
          zipWithM_
             (\k (ia,ib) -> do
                PackST.writeMatrix mat k ia (-1)
                PackST.writeMatrix mat k ib 1)
             [0..] is
          return mat
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
   [(Maybe Float, Maybe Float)] ->
   [((Int, (Float, Float)), (Int, (Float, Float)))] ->
   ([((Double,Double), Complex Double)],
    [(Double,Double)])
layoutFromPairDisplacements mxys correspondences =
   let numPics = length mxys
       {-
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
       (solution, projection) =
          leastSquaresSelected matrix
             (concatMap
                (\((mx,my), mr) ->
                   [(/weight) . realToFrac <$> mx,
                    (/weight) . realToFrac <$> my,
                    fst <$> mr, snd <$> mr]) $
              zip mxys $ Just (1,0) : repeat Nothing)
             (Container.constant 0 (2 * length correspondences))
   in  (map (\[dx,dy,rx,ry] -> ((weight*dx,weight*dy), rx :+ ry)) $
        ListHT.sliceVertical 4 solution,
        map (\[x,y] -> (x,y)) $
        ListHT.sliceVertical 2 projection)
