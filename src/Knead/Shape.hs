{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Knead.Shape where

import qualified Data.Array.Knead.Shape as Shape
import qualified Data.Array.Knead.Expression as Expr

import qualified Data.Array.Comfort.Shape as ComfortShape

import qualified LLVM.Extra.Marshal as Marshal
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Tuple as Tuple
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

import Data.Tuple.HT (mapSnd)
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
type Dim1 = Shape.ZeroBased Size
type Dim2 = Shape2ZB Size
type Ix2  = Index2 Size

data Vec2 tag i = Vec2 {vertical, horizontal :: i}

data ShapeTag
data IndexTag
data FactorTag

type Shape2 = Vec2 ShapeTag
type Shape2ZB n = Shape2 (Shape.ZeroBased n)
type Index2 = Vec2 IndexTag
type Factor2 = Vec2 FactorTag



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

instance (Tuple.Undefined n) => Tuple.Undefined (Vec2 tag n) where
   undef = Vec2 Tuple.undef Tuple.undef

instance (Tuple.Phi n) => Tuple.Phi (Vec2 tag n) where
   phi bb (Vec2 a0 a1) =
      Monad.lift2 Vec2 (Tuple.phi bb a0) (Tuple.phi bb a1)
   addPhi bb (Vec2 a0 a1) (Vec2 b0 b1) =
      Tuple.addPhi bb a0 b0 >>
      Tuple.addPhi bb a1 b1

instance (Tuple.Value n) => Tuple.Value (Vec2 tag n) where
   type ValueOf (Vec2 tag n) = Vec2 tag (Tuple.ValueOf n)
   valueOf (Vec2 n m) = Vec2 (Tuple.valueOf n) (Tuple.valueOf m)

instance (MultiValue.C n) => MultiValue.C (Vec2 tag n) where
   cons (Vec2 n m) =
      MultiValue.compose $ Vec2 (MultiValue.cons n) (MultiValue.cons m)
   undef = MultiValue.compose $ squareShape MultiValue.undef
   zero = MultiValue.compose $ squareShape MultiValue.zero
   phi bb a =
      case MultiValue.decompose (squareShape atom) a of
         Vec2 a0 a1 ->
            fmap MultiValue.compose $
            Monad.lift2 Vec2 (MultiValue.phi bb a0) (MultiValue.phi bb a1)
   addPhi bb a b =
      case (MultiValue.decompose (squareShape atom) a,
            MultiValue.decompose (squareShape atom) b) of
         (Vec2 a0 a1, Vec2 b0 b1) ->
            MultiValue.addPhi bb a0 b0 >>
            MultiValue.addPhi bb a1 b1

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

instance (Memory.C i) => Memory.C (Vec2 tag i) where
   type Struct (Vec2 tag i) =
            LLVM.Struct (Memory.Struct i, (Memory.Struct i, ()))
   decompose nm =
      Monad.lift2 Vec2
         (Memory.decompose =<< LLVM.extractvalue nm TypeNum.d0)
         (Memory.decompose =<< LLVM.extractvalue nm TypeNum.d1)
   compose (Vec2 n m) = do
      sn <- Memory.compose n
      sm <- Memory.compose m
      rn <- LLVM.insertvalue (LLVM.value LLVM.undef) sn TypeNum.d0
      LLVM.insertvalue rn sm TypeNum.d1

instance (Marshal.C i) => Marshal.C (Vec2 tag i) where
   pack (Vec2 n m) = LLVM.consStruct (Marshal.pack n) (Marshal.pack m)
   unpack =
      LLVM.uncurryStruct $ \n m -> Vec2 (Marshal.unpack n) (Marshal.unpack m)

instance (Marshal.C i, MultiValue.C i) => Marshal.MV (Vec2 tag i) where


unzipShape :: MultiValue.T (Vec2 tag n) -> Vec2 tag (MultiValue.T n)
unzipShape = MultiValue.decompose (squareShape atom)

zipShape :: MultiValue.T n -> MultiValue.T n -> MultiValue.T (Vec2 tag n)
zipShape y x = MultiValue.compose $ Vec2 y x

instance (tag ~ ShapeTag, ComfortShape.C i) => ComfortShape.C (Vec2 tag i) where
   size (Vec2 n m) = ComfortShape.size n * ComfortShape.size m

instance
   (tag ~ ShapeTag, ComfortShape.Indexed i) =>
      ComfortShape.Indexed (Vec2 tag i) where
   type Index (Vec2 tag i) = Index2 (Shape.Index i)
   indices (Vec2 n m) = map (uncurry Vec2) $ ComfortShape.indices (n,m)
   sizeOffset (Vec2 n m) =
      mapSnd (. (\(Vec2 i j) -> (i,j))) $ ComfortShape.sizeOffset (n,m)
   inBounds (Vec2 n m) (Vec2 i j) = ComfortShape.inBounds (n,m) (i,j)

instance (tag ~ ShapeTag, Shape.C i) => Shape.C (Vec2 tag i) where
   intersectCode a b =
      case (unzipShape a, unzipShape b) of
         (Vec2 an am, Vec2 bn bm) ->
            Monad.lift2 zipShape
               (Shape.intersectCode an bn)
               (Shape.intersectCode am bm)
   size nm =
      case unzipShape nm of
         Vec2 n m ->
            join $ Monad.lift2 A.mul (Shape.size n) (Shape.size m)
   sizeOffset nm =
      case unzipShape nm of
         (Vec2 n m) -> do
            (ns, iOffset) <- Shape.sizeOffset n
            (ms, jOffset) <- Shape.sizeOffset m
            sz <- A.mul ns ms
            return
               (sz,
                \ij ->
                  case unzipShape ij of
                     (Vec2 i j) -> do
                        il <- iOffset i
                        jl <- jOffset j
                        A.add jl =<< A.mul ms il)
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

verticalSize, horizontalSize ::
   (Expr.Value val) => val (Shape2 (Shape.ZeroBased n)) -> val n
verticalSize = Shape.zeroBasedSize . verticalVal
horizontalSize = Shape.zeroBasedSize . horizontalVal
