module Knead.CArray where

import qualified Arithmetic as Arith

import qualified Math.FFT as FFT
import Math.FFT.Base (FFTWReal)

import Foreign.Storable (Storable)

import qualified Data.Array.CArray as CArray
import Data.Array.IArray (amap)
import Data.Array.CArray (CArray)

import Data.Complex (Complex((:+)), conjugate, realPart)

import Data.Tuple.HT (mapPair)


pad ::
   (Storable a) => a -> (Int,Int) -> CArray (Int,Int) a -> CArray (Int,Int) a
pad a (height, width) img =
   CArray.listArray ((0,0), (height-1, width-1)) (repeat a)
   CArray.//
   CArray.assocs img

clip :: (Storable a) => (Int,Int) -> CArray (Int,Int) a -> CArray (Int,Int) a
clip (height, width) = CArray.ixmap ((0,0), (height-1, width-1)) id

correlatePaddedSimple ::
   (FFTWReal a) =>
   (Int,Int) ->
   CArray (Int,Int) a ->
   CArray (Int,Int) a ->
   CArray (Int,Int) a
correlatePaddedSimple sh =
   let forward = FFT.dftRCN [0,1] . pad 0 sh
       inverse = FFT.dftCRN [0,1]
   in  \ a b ->
         inverse $ CArray.liftArray2 Arith.mulConj (forward a) (forward b)

-- expects zero-based arrays
cyclicReverse2d :: (Storable a) => CArray (Int,Int) a -> CArray (Int,Int) a
cyclicReverse2d spec =
   let (height, width) = mapPair ((1+), (1+)) $ snd $ CArray.bounds spec
   in  CArray.ixmap (CArray.bounds spec)
         (\(y,x) -> (mod (-y) height, mod (-x) width)) spec

untangleCoefficient ::
   (RealFloat a) => Complex a -> Complex a -> (Complex a, Complex a)
untangleCoefficient a b =
   let bc = conjugate b
   in  ((a + bc) / 2, (a - bc) * (0 :+ (-1/2)))

-- ToDo: could be moved to fft package
untangleSpectra2d ::
   (RealFloat a, Storable a) =>
   CArray (Int,Int) (Complex a) -> CArray (Int,Int) (Complex a, Complex a)
untangleSpectra2d spec =
   CArray.liftArray2 untangleCoefficient spec (cyclicReverse2d spec)

{- |
Equivalent to @amap (uncurry Arith.mulConj) . untangleSpectra2d@
but much faster, since it avoids the slow @instance Storable (a,b)@
based on @storable-tuple:storePair@.
-}
mulConjUntangledSpectra2d ::
   (RealFloat a, Storable a) =>
   CArray (Int,Int) (Complex a) -> CArray (Int,Int) (Complex a)
mulConjUntangledSpectra2d spec =
   CArray.liftArray2
      ((uncurry Arith.mulConj .) . untangleCoefficient)
      spec (cyclicReverse2d spec)


{-
This is more efficient than 'correlatePaddedSimpleCArray'
since it needs only one complex forward Fourier transform,
where 'correlatePaddedSimpleCArray' needs two real transforms.
Especially for odd sizes
two real transforms are slower than a complex transform.
For the analysis part,
perform two real-valued Fourier transforms using one complex-valued transform.
Afterwards we untangle the superposed spectra.
-}
correlatePaddedComplex ::
   (FFTWReal a) =>
   (Int,Int) ->
   CArray (Int,Int) a ->
   CArray (Int,Int) a ->
   CArray (Int,Int) a
correlatePaddedComplex sh a b =
   amap realPart $ FFT.idftN [0,1] $
   mulConjUntangledSpectra2d $ FFT.dftN [0,1] $
   CArray.liftArray2 (:+) (pad 0 sh a) (pad 0 sh b)

{- |
Should be yet a little bit more efficient than 'correlatePaddedComplexCArray'
since it uses a real back transform.
-}
correlatePadded ::
   (FFTWReal a) =>
   (Int,Int) ->
   CArray (Int,Int) a ->
   CArray (Int,Int) a ->
   CArray (Int,Int) a
correlatePadded sh@(height,width) a b =
   (case divMod width 2 of
      (halfWidth,0) -> FFT.dftCRN [0,1] . clip (height,halfWidth+1)
      (halfWidth,_) -> FFT.dftCRON [0,1] . clip (height,halfWidth+1)) $
   mulConjUntangledSpectra2d $ FFT.dftN [0,1] $
   CArray.liftArray2 (:+) (pad 0 sh a) (pad 0 sh b)
