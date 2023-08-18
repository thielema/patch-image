{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Knead.CArray where

import qualified Complex as Komplex

import qualified Numeric.FFTW.Rank2 as Trafo2
import qualified Numeric.FFTW.Shape as Spectrum
import qualified Numeric.Netlib.Class as Class

import Foreign.Storable.Record.Tuple (Tuple(Tuple))
import Foreign.Storable (Storable)

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array)
import Data.Array.Comfort.Storable ((!))

import Data.Complex (Complex((:+)), realPart)

import Data.Maybe (fromMaybe)


type Plane = Array (Shape.ZeroBased Int, Shape.ZeroBased Int)
type Cyclic = Array (Shape.Cyclic Int, Shape.Cyclic Int)


pad :: (Storable a) => a -> (Int,Int) -> Plane a -> Cyclic a
pad a (height, width) img =
   Array.sample (Shape.Cyclic height, Shape.Cyclic width) $ \ix ->
      fromMaybe a $ Array.accessMaybe img ix

uncycle :: Cyclic a -> Plane a
uncycle =
   Array.mapShape
      (\(Shape.Cyclic height, Shape.Cyclic width) ->
         (Shape.ZeroBased height, Shape.ZeroBased width))

clip ::
   (Storable a, Shape.Indexed sh0, Shape.Indexed sh1) =>
   (Shape.Index sh0 ~ Shape.Index sh1) =>
   sh1 -> Array sh0 a -> Array sh1 a
clip sh img = Array.sample sh (img!)

correlatePaddedSimple ::
   (Class.Real a) => (Int,Int) -> Plane a -> Plane a -> Plane a
correlatePaddedSimple sh =
   let forward = Trafo2.fourierRC . pad 0 sh
       inverse = Trafo2.fourierCR
   in \a b ->
         uncycle $ Array.map (/ fromIntegral (uncurry (*) sh)) $
         inverse $
         Array.zipWith Komplex.mulConj (forward a) (forward b)

cyclicReverse2d :: (Storable a) => Cyclic a -> Cyclic a
cyclicReverse2d spec =
   let shape@(Shape.Cyclic height, Shape.Cyclic width) = Array.shape spec
   in Array.sample shape (\(y,x) -> spec ! (mod (-y) height, mod (-x) width))

untangleCoefficient ::
   (Fractional a) => Complex a -> Complex a -> (Complex a, Complex a)
untangleCoefficient a b =
   let bc = Komplex.conjugate b
   in  (Komplex.mul (Komplex.add a bc) ((1/2) :+ 0),
        Komplex.mul (Komplex.sub a bc) (0 :+ (-1/2)))

untangleCoefficient_ ::
   (RealFloat a) => Complex a -> Complex a -> (Complex a, Complex a)
untangleCoefficient_ a b =
   let bc = Komplex.conjugate b
   in  ((a + bc) / 2, (a - bc) * (0 :+ (-1/2)))

-- ToDo: could be moved to fft package
untangleSpectra2d ::
   (Fractional a, Storable a) =>
   Cyclic (Complex a) ->
   Cyclic (Tuple (Complex a, Complex a))
untangleSpectra2d spec =
   Array.zipWith
      ((Tuple.) . untangleCoefficient)
      spec (cyclicReverse2d spec)

{- |
Equivalent to @amap (uncurry Komplex.mulConj) . untangleSpectra2d@
but much faster, since it avoids the slow @instance Storable (a,b)@
based on @storable-tuple:storePair@.
-}
mulConjUntangledSpectra2d ::
   (Fractional a, Storable a) =>
   Cyclic (Complex a) -> Cyclic (Complex a)
mulConjUntangledSpectra2d spec =
   Array.zipWith
      ((uncurry Komplex.mulConj .) . untangleCoefficient)
      spec (cyclicReverse2d spec)


{-
This is more efficient than 'correlatePaddedSimple'
since it needs only one complex forward Fourier transform,
where 'correlatePaddedSimple' needs two real transforms.
Especially for odd sizes
two real transforms are slower than a complex transform.
For the analysis part,
perform two real-valued Fourier transforms using one complex-valued transform.
Afterwards we untangle the superposed spectra.
-}
correlatePaddedComplex ::
   (Class.Real a) => (Int,Int) -> Plane a -> Plane a -> Plane a
correlatePaddedComplex sh a b =
   uncycle $ Array.map ((/ fromIntegral (uncurry (*) sh)) . realPart) $
   Trafo2.fourier Trafo2.Backward $
   mulConjUntangledSpectra2d $ Trafo2.fourier Trafo2.Forward $
   Array.zipWith (:+) (pad 0 sh a) (pad 0 sh b)

{- |
Should be yet a little bit more efficient than 'correlatePaddedComplex'
since it uses a real back transform.
-}
correlatePadded ::
   (Class.Real a) => (Int,Int) -> Plane a -> Plane a -> Plane a
correlatePadded sh@(height,width) a b =
   uncycle $ Array.map (/ fromIntegral (height*width)) $
   Trafo2.fourierCR $
   clip (Shape.Cyclic height, Spectrum.Half width) $
   mulConjUntangledSpectra2d $ Trafo2.fourier Trafo2.Forward $
   Array.zipWith (:+) (pad 0 sh a) (pad 0 sh b)
