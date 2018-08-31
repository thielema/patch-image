module Degree where

import qualified Arithmetic as Arith

import qualified Data.Csv as Csv
import Data.Monoid (Monoid, mempty, mappend)
import Data.Semigroup (Semigroup, (<>))


newtype Degree a = Degree {getDegree :: a}
   deriving (Eq, Show)

instance (Num a) => Semigroup (Degree a) where
   Degree x <> Degree y = Degree $ x+y

instance (Num a) => Monoid (Degree a) where
   mempty = Degree 0
   mappend = (<>)

instance Functor Degree where
   fmap f (Degree x) = Degree $ f x

instance Csv.FromField a => Csv.FromField (Degree a) where
   parseField s = fmap Degree $ Csv.parseField s

toRadian :: (Floating a) => Degree a -> a
toRadian (Degree angle) = angle*pi/180

fromRadian :: (Floating a) => a -> Degree a
fromRadian angle = Degree $ angle*180/pi

linearScale :: (Fractional a) => Int -> Degree a -> [Degree a]
linearScale num (Degree maxAngle) =
   map Degree $ Arith.linearScale num (-maxAngle, maxAngle)

cis :: (Floating a) => Degree a -> (a,a)
cis deg =
   case toRadian deg of
      rad -> (cos rad, sin rad)
