{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Knead.Shape where

import qualified Data.Array.Knead.Shape.Nested as Shape
import qualified Data.Array.Knead.Expression as Expr

import qualified LLVM.Extra.Multi.Value.Memory as MultiMem
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Iterator as Iter
import qualified LLVM.Extra.Arithmetic as A
import LLVM.Extra.Multi.Value (atom)

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import Foreign.Storable
         (Storable, sizeOf, alignment, poke, pokeElemOff, peek, peekElemOff)
import Foreign.Ptr (Ptr, castPtr)

import qualified Control.Monad.HT as Monad
import Control.Monad (join)

import Data.Int (Int64)


{- |
I choose a bit complicated Dim2 definition
to make it distinct from size pairs with width and height swapped.
Alternatives would be Index.Linear or intentionally complicated Shape types like:

type Dim0 = ()
type Dim1 = ((), Size)
type Dim2 = ((), Size, Size)

Problems with Index.Linear is that it is fixed to Word32 dimensions
which causes trouble with negative coordinates
that we encounter on rotations.

The custom shape type requires lots of new definitions
but it is certainly the cleanest solution.
-}
type Size = Int64
type Dim0 = ()
type Dim1 = Size
type Dim2 = Shape2 Size
type Ix2  = Index2 Size

data Vec2 tag i = Vec2 {vertical, horizontal :: i}

data ShapeTag
data IndexTag

type Shape2 = Vec2 ShapeTag
type Index2 = Vec2 IndexTag



squareShape :: n -> Vec2 tag n
squareShape n = Vec2 n n

castToElemPtr :: Ptr (Vec2 tag a) -> Ptr a
castToElemPtr = castPtr

instance (Storable n) => Storable (Vec2 tag n) where
   -- cf. sample-frame:Frame.Stereo
   sizeOf ~(Vec2 n m) =
      sizeOf n + mod (- sizeOf n) (alignment m) + sizeOf m
   alignment ~(Vec2 n _) = alignment n
   poke p (Vec2 n m) =
      let q = castToElemPtr p
      in  poke q n >> pokeElemOff q 1 m
   peek p =
      let q = castToElemPtr p
      in  Monad.lift2 Vec2 (peek q) (peekElemOff q 1)

instance (MultiValue.C n) => MultiValue.C (Vec2 tag n) where
   type Repr f (Vec2 tag n) = Vec2 tag (MultiValue.Repr f n)
   cons (Vec2 n m) =
      MultiValue.compose $ Vec2 (MultiValue.cons n) (MultiValue.cons m)
   undef = MultiValue.compose $ squareShape MultiValue.undef
   zero = MultiValue.compose $ squareShape MultiValue.zero
   phis bb a =
      case MultiValue.decompose (squareShape atom) a of
         Vec2 a0 a1 ->
            fmap MultiValue.compose $
            Monad.lift2 Vec2 (MultiValue.phis bb a0) (MultiValue.phis bb a1)
   addPhis bb a b =
      case (MultiValue.decompose (squareShape atom) a,
            MultiValue.decompose (squareShape atom) b) of
         (Vec2 a0 a1, Vec2 b0 b1) ->
            MultiValue.addPhis bb a0 b0 >>
            MultiValue.addPhis bb a1 b1

type instance
   MultiValue.Decomposed f (Vec2 tag pat) =
      Vec2 tag (MultiValue.Decomposed f pat)
type instance
   MultiValue.PatternTuple (Vec2 tag pat) =
      Vec2 tag (MultiValue.PatternTuple pat)

instance (MultiValue.Compose n) => MultiValue.Compose (Vec2 tag n) where
   type Composed (Vec2 tag n) = Vec2 tag (MultiValue.Composed n)
   compose (Vec2 n m) =
      case (MultiValue.compose n, MultiValue.compose m) of
         (MultiValue.Cons rn, MultiValue.Cons rm) ->
            MultiValue.Cons (Vec2 rn rm)

instance (MultiValue.Decompose pn) => MultiValue.Decompose (Vec2 tag pn) where
   decompose (Vec2 pn pm) (MultiValue.Cons (Vec2 n m)) =
      Vec2
         (MultiValue.decompose pn (MultiValue.Cons n))
         (MultiValue.decompose pm (MultiValue.Cons m))

instance (MultiMem.C i) => MultiMem.C (Vec2 tag i) where
   type Struct (Vec2 tag i) =
         LLVM.Struct (MultiMem.Struct i, (MultiMem.Struct i, ()))
   decompose nm =
      Monad.lift2 zipShape
         (MultiMem.decompose =<< LLVM.extractvalue nm TypeNum.d0)
         (MultiMem.decompose =<< LLVM.extractvalue nm TypeNum.d1)
   compose nm =
      case unzipShape nm of
         Vec2 n m -> do
            sn <- MultiMem.compose n
            sm <- MultiMem.compose m
            rn <- LLVM.insertvalue (LLVM.value LLVM.undef) sn TypeNum.d0
            LLVM.insertvalue rn sm TypeNum.d1


unzipShape :: MultiValue.T (Vec2 tag n) -> Vec2 tag (MultiValue.T n)
unzipShape = MultiValue.decompose (squareShape atom)

zipShape :: MultiValue.T n -> MultiValue.T n -> MultiValue.T (Vec2 tag n)
zipShape y x = MultiValue.compose $ Vec2 y x

instance (tag ~ ShapeTag, Shape.C i) => Shape.C (Vec2 tag i) where
   type Index (Vec2 tag i) = Index2 (Shape.Index i)
   intersectCode a b =
      case (unzipShape a, unzipShape b) of
         (Vec2 an am, Vec2 bn bm) ->
            Monad.lift2 zipShape
               (Shape.intersectCode an bn)
               (Shape.intersectCode am bm)
   sizeCode nm =
      case unzipShape nm of
         Vec2 n m ->
            join $ Monad.lift2 A.mul (Shape.sizeCode n) (Shape.sizeCode m)
   size (Vec2 n m) = Shape.size n * Shape.size m
   flattenIndexRec nm ij =
      case (unzipShape nm, unzipShape ij) of
         (Vec2 n m, Vec2 i j) -> do
            (ns, il) <- Shape.flattenIndexRec n i
            (ms, jl) <- Shape.flattenIndexRec m j
            Monad.lift2 (,)
               (A.mul ns ms)
               (A.add jl =<< A.mul ms il)
   loop code nm =
      case unzipShape nm of
         Vec2 n m ->
            Shape.loop (\i -> Shape.loop (\j -> code (zipShape i j)) m) n
   iterator nm =
      case unzipShape nm of
         Vec2 n m ->
            fmap (uncurry zipShape) $
            Iter.cartesian (Shape.iterator n) (Shape.iterator m)


instance (Expr.Compose n) => Expr.Compose (Vec2 tag n) where
   type Composed (Vec2 tag n) = Vec2 tag (Expr.Composed n)
   compose (Vec2 n m) = Expr.lift2 zipShape (Expr.compose n) (Expr.compose m)

instance (Expr.Decompose p) => Expr.Decompose (Vec2 tag p) where
   decompose (Vec2 pn pm) vec =
      Vec2
         (Expr.decompose pn (verticalVal vec))
         (Expr.decompose pm (horizontalVal vec))

verticalVal, horizontalVal :: (Expr.Value val) => val (Vec2 tag n) -> val n
verticalVal = Expr.lift1 (MultiValue.lift1 vertical)
horizontalVal = Expr.lift1 (MultiValue.lift1 horizontal)
