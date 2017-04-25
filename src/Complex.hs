module Complex where

import qualified Data.Complex as HComplex
import Data.Complex (Complex((:+)))


toPair :: (RealFloat a) => Complex a -> (a,a)
toPair z = (HComplex.realPart z, HComplex.imagPart z)

map :: (a -> b) -> Complex a -> Complex b
map f (r :+ i)  =  f r :+ f i

conjugate :: (Num a) => Complex a -> Complex a
conjugate (r :+ i)  =  r :+ negate i

add :: (Num a) => Complex a -> Complex a -> Complex a
add (xr:+xi) (yr:+yi) = (xr+yr) :+ (xi+yi)

sub :: (Num a) => Complex a -> Complex a -> Complex a
sub (xr:+xi) (yr:+yi) = (xr-yr) :+ (xi-yi)

mul :: (Num a) => Complex a -> Complex a -> Complex a
mul (xr:+xi) (yr:+yi) = (xr*yr-xi*yi) :+ (xr*yi+xi*yr)

mulConj :: (Num a) => Complex a -> Complex a -> Complex a
mulConj x y = mul x $ conjugate y

mulConj_ :: (RealFloat a) => Complex a -> Complex a -> Complex a
mulConj_ x y = x * Complex.conjugate y
