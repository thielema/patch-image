{-# LANGUAGE TypeFamilies #-}
{- |
This is an approach for stitching images at narrow bands
along lines of small image differences.
We start with rotated and placed rectangular image masks
and then start to remove pixels from the borders of the image masks
in the order of decreasing pixel value differences.
For the sake of simplicity we calculate the difference
of a pixel value to the average of all pixel values at a certain position.
We do not recalculate the average if a pixel is removed from the priority queue.
-}
module MatchImageBorders where

import Knead.Shape (Vec2(Vec2), Dim2, Size)

import qualified Data.Array.Comfort.Storable.Mutable.Internal as MutArray
import qualified Data.Array.Comfort.Storable.Internal as Array
import qualified Data.Array.Comfort.Shape as ComfortShape
import Data.Array.Comfort.Storable.Mutable (IOArray)
import Data.Array.Comfort.Storable (Array, (!), (//))

import qualified Data.PQueue.Prio.Max as PQ
import qualified Data.Set as Set
import Data.PQueue.Prio.Max (MaxPQueue)
import Data.Set (Set)

import qualified Data.Bool8 as Bool8
import Data.Traversable (forM)
import Data.Foldable (forM_)
import Data.Tuple.HT (mapPair, mapSnd)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word8)
import Data.Bool8 (Bool8)

import Control.Monad (filterM)
import Control.Applicative ((<$>))


type Z2 i j = (ComfortShape.ZeroBased i, ComfortShape.ZeroBased j)

arrayPairFromVec :: Array Dim2 a -> Array (Z2 Size Size) a
arrayPairFromVec = Array.mapShape (\(Vec2 height width) -> (height, width))

arrayVecFromPair :: Array (Z2 Size Size) a -> Array Dim2 a
arrayVecFromPair = Array.mapShape (\(height, width) -> (Vec2 height width))


findBorder :: (Integral i, Integral j) => Array (Z2 i j) Bool8 -> Set (i,j)
findBorder mask =
   let (yrng, xrng) = Array.shape mask
       range sh = ComfortShape.indices sh
       revRange sh@(ComfortShape.ZeroBased n) =
          take (ComfortShape.size sh) $ drop 1 $ iterate (subtract 1) n
       first p = listToMaybe . dropWhile (not . Bool8.toBool . p)
       findLeft   y = first (\x -> mask!(y,x)) $ range xrng
       findRight  y = first (\x -> mask!(y,x)) $ revRange xrng
       findTop    x = first (\y -> mask!(y,x)) $ range yrng
       findBottom x = first (\y -> mask!(y,x)) $ revRange yrng
   in  Set.fromList $
         mapMaybe (\y -> (,) y <$> findLeft y) (range yrng) ++
         mapMaybe (\y -> (,) y <$> findRight y) (range yrng) ++
         mapMaybe (\x -> flip (,) x <$> findTop x) (range xrng) ++
         mapMaybe (\x -> flip (,) x <$> findBottom x) (range xrng)

pqueueFromBorder ::
   (ComfortShape.Indexed sh, ComfortShape.Index sh ~ ix) =>
   Array sh Float -> Set ix -> MaxPQueue Float ix
pqueueFromBorder weights =
   PQ.fromList . map (\pos -> (weights!pos, pos)) . Set.toList


type Location = Word8

locOutside, locBorder, locInside :: Location
locOutside = 0
locBorder = 1
locInside = 2

prepareLocations ::
   (ComfortShape.Indexed sh, ComfortShape.Index sh ~ ix) =>
   Array sh Bool8 -> Set ix -> Array sh Location
prepareLocations mask border =
   Array.map (\b -> if Bool8.toBool b then locInside else locOutside) mask
   //
   map (flip (,) locBorder) (Set.toList border)


type
   Queue i j =
      MaxPQueue Float
         ((IOArray (Z2 i j) Location, Array (Z2 i j) Float), (i,j))

prepareShaping ::
   (Integral i, Integral j) =>
   [(Array (Z2 i j) Bool8, Array (Z2 i j) Float)] ->
   IO ([IOArray (Z2 i j) Location], Queue i j)
prepareShaping maskWeightss =
   fmap (mapSnd PQ.unions . unzip) $
   forM maskWeightss $ \(mask, weights) -> do
      let border = findBorder mask
      locations <- MutArray.thaw $ prepareLocations mask border
      return
         (locations,
          fmap ((,) (locations, weights)) $ pqueueFromBorder weights border)


loopQueue ::
   (Monad m, Ord k) => (a -> m (MaxPQueue k a)) -> MaxPQueue k a -> m ()
loopQueue f =
   let loop queue =
         case PQ.maxView queue of
            Nothing -> return ()
            Just (first, remQueue) -> loop . PQ.union remQueue =<< f first
   in loop

shapeParts ::
   IOArray (Z2 Size Size) Int ->
   [IOArray (Z2 Size Size) Location] ->
   Queue Size Size -> IO [Array (Z2 Size Size) Bool8]
shapeParts count masks queue = do
   flip loopQueue queue $ \((locs, diffs), pos@(y,x)) -> do
      n <- MutArray.read count pos
      if n<=1
        then return PQ.empty
        else do
            MutArray.write count pos (n-1)
            MutArray.write locs pos locOutside
            envPoss <-
               filterM (fmap (locInside ==) . MutArray.read locs) $
               filter (ComfortShape.inBounds (Array.shape diffs)) $
               map (mapPair ((y+), (x+))) [(0,1), (1,0), (0,-1), (-1,0)]
            forM_ envPoss $ \envPos -> MutArray.write locs envPos locBorder
            return $ PQ.fromList $
               map (\envPos -> (diffs!envPos, ((locs, diffs), envPos))) envPoss

   forM masks $
      fmap (Array.map (Bool8.fromBool . (/=locOutside))) . MutArray.freeze
