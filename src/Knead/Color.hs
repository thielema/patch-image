{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Knead.Color where

import qualified Data.Array.Knead.Expression as Expr
import Data.Array.Knead.Expression (Exp)

import qualified LLVM.Extra.Storable as Storable
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Vector as Vector
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Util.Proxy as LP
import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Foreign.Storable.Traversable as StoreTrav
import Foreign.Storable (Storable, sizeOf, alignment, poke, peek)

import Control.Applicative (Applicative, liftA3, pure, (<*>))

import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))



data YUV a = YUV a a a

instance Functor YUV where
   fmap f (YUV y u v) = YUV (f y) (f u) (f v)

instance Foldable YUV where
   foldMap f (YUV y u v) = f y <> f u <> f v

instance Traversable YUV where
   traverse f (YUV y u v) = liftA3 YUV (f y) (f u) (f v)

instance Applicative YUV where
   pure a = YUV a a a
   YUV fy fu fv <*> YUV y u v = YUV (fy y) (fu u) (fv v)


instance (Storable a) => Storable (YUV a) where
   sizeOf = StoreTrav.sizeOf . lazyElements
   alignment = StoreTrav.alignment
   peek = StoreTrav.peekApplicative
   poke = StoreTrav.poke

lazyElements :: YUV a -> YUV a
lazyElements ~(YUV y u v) = YUV y u v

instance
   (Storable.Vector a, LLVM.IsPrimitive a, LLVM.IsConst a,
    Tuple.VectorValue TypeNum.D3 a,
    Tuple.VectorValueOf TypeNum.D3 a ~ LLVM.Value (LLVM.Vector TypeNum.D3 a)) =>
      Storable.C (YUV a) where
   type Struct (YUV a) = Storable.Struct a
   load = Storable.load . vectorProxy
   store = Storable.store . vectorProxy

vectorProxy :: LP.Proxy (YUV a) -> LP.Proxy (LLVM.Vector TypeNum.D3 a)
vectorProxy LP.Proxy = LP.Proxy


instance
   (LLVM.IsPrimitive a, LLVM.IsConst a) =>
      Tuple.Value (YUV a) where
   type ValueOf (YUV a) = LLVM.Value (LLVM.Vector TypeNum.D3 a)
   valueOf (YUV a0 a1 a2) = LLVM.valueOf $ LLVM.consVector a0 a1 a2

instance
   (LLVM.IsPrimitive a, LLVM.IsConst a) =>
      MultiValue.C (YUV a) where
   cons = MultiValue.consTuple
   undef = MultiValue.undefTuple
   zero = MultiValue.zeroTuple
   phi = MultiValue.phiTuple
   addPhi = MultiValue.addPhiTuple


yuv ::
   (LLVM.IsPrimitive a, Tuple.ValueOf a ~ LLVM.Value a) =>
   Exp a -> Exp a -> Exp a -> Exp (YUV a)
yuv =
   Expr.liftTupleM3
      (\y u v -> do
         arr0 <- LLVM.insertelement Tuple.undef y (LLVM.valueOf 0)
         arr1 <- LLVM.insertelement arr0 u (LLVM.valueOf 1)
         LLVM.insertelement arr1 v (LLVM.valueOf 2))

brightness ::
   (LLVM.IsPrimitive a, Tuple.ValueOf a ~ LLVM.Value a) =>
   Exp (YUV a) -> Exp a
brightness =
   Expr.liftTupleM (flip LLVM.extractelement (LLVM.valueOf 0))

mapPlain ::
   (LLVM.IsPrimitive a, LLVM.IsPrimitive b) =>
   (forall r. LLVM.Value a -> LLVM.CodeGenFunction r (LLVM.Value b)) ->
   Exp (YUV a) -> Exp (YUV b)
mapPlain f = Expr.liftTupleM (Vector.map f)

exprUnliftM1 ::
   (Tuple.ValueOf a ~ al, Tuple.ValueOf b ~ bl) =>
   (Exp a -> Exp b) -> al -> LLVM.CodeGenFunction r bl
exprUnliftM1 f a =
   fmap (\(MultiValue.Cons b) -> b) $ Expr.unliftM1 f $ MultiValue.Cons a

map ::
   (LLVM.IsPrimitive a, Tuple.ValueOf a ~ LLVM.Value a,
    LLVM.IsPrimitive b, Tuple.ValueOf b ~ LLVM.Value b) =>
   (Exp a -> Exp b) -> Exp (YUV a) -> Exp (YUV b)
map f = mapPlain (exprUnliftM1 f)
