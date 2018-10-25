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

import qualified Data.PQueue.Prio.Max as PQ
import qualified Data.Set as Set
import Data.PQueue.Prio.Max (MaxPQueue)
import Data.Set (Set)

import qualified Data.Array.CArray.Base as CArrayPriv
import Data.Array.IOCArray (IOCArray)
import Data.Array.MArray (readArray, writeArray, thaw)
import Data.Array.CArray (CArray)
import Data.Array.IArray (Ix, amap, bounds, range, inRange, (!), (//))

import qualified Data.Bool8 as Bool8
import Data.Traversable (forM)
import Data.Foldable (forM_)
import Data.Tuple.HT (mapPair, mapSnd)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word8)
import Data.Bool8 (Bool8)

import Control.Monad (filterM)
import Control.Applicative ((<$>))


findBorder :: (Ix i, Enum i, Ix j, Enum j) => CArray (i,j) Bool8 -> Set (i,j)
findBorder mask =
   let ((yl,xl), (yu,xu)) = bounds mask
       revRange (l,u) = [u, pred u .. l]
       first p = listToMaybe . dropWhile (not . Bool8.toBool . p)
       findLeft   y = first (\x -> mask!(y,x)) $ range (xl,xu)
       findRight  y = first (\x -> mask!(y,x)) $ revRange (xl,xu)
       findTop    x = first (\y -> mask!(y,x)) $ range (yl,yu)
       findBottom x = first (\y -> mask!(y,x)) $ revRange (yl,yu)
   in  Set.fromList $
         mapMaybe (\y -> (,) y <$> findLeft y) (range (yl,yu)) ++
         mapMaybe (\y -> (,) y <$> findRight y) (range (yl,yu)) ++
         mapMaybe (\x -> flip (,) x <$> findTop x) (range (xl,xu)) ++
         mapMaybe (\x -> flip (,) x <$> findBottom x) (range (xl,xu))

pqueueFromBorder :: (Ix ix) => CArray ix Float -> Set ix -> MaxPQueue Float ix
pqueueFromBorder weights =
   PQ.fromList . map (\pos -> (weights!pos, pos)) . Set.toList


type Location = Word8

locOutside, locBorder, locInside :: Location
locOutside = 0
locBorder = 1
locInside = 2

prepareLocations :: (Ix ix) => CArray ix Bool8 -> Set ix -> CArray ix Location
prepareLocations mask border =
   amap (\b -> if Bool8.toBool b then locInside else locOutside) mask
   //
   map (flip (,) locBorder) (Set.toList border)


type
   Queue i j =
      MaxPQueue Float ((IOCArray (i,j) Location, CArray (i,j) Float), (i,j))

prepareShaping ::
   (Ix i, Enum i, Ix j, Enum j) =>
   [(CArray (i,j) Bool8, CArray (i,j) Float)] ->
   IO ([IOCArray (i,j) Location], Queue i j)
prepareShaping maskWeightss =
   fmap (mapSnd PQ.unions . unzip) $
   forM maskWeightss $ \(mask, weights) -> do
      let border = findBorder mask
      locations <- thaw $ prepareLocations mask border
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
   IOCArray (Int, Int) Int ->
   [IOCArray (Int, Int) Location] ->
   Queue Int Int -> IO [CArray (Int, Int) Bool8]
shapeParts count masks queue = do
   flip loopQueue queue $ \((locs, diffs), pos@(y,x)) -> do
      n <- readArray count pos
      if n<=1
        then return PQ.empty
        else do
            writeArray count pos (n-1)
            writeArray locs pos locOutside
            envPoss <-
               filterM (fmap (locInside ==) . readArray locs) $
               filter (inRange (bounds diffs)) $
               map (mapPair ((y+), (x+))) [(0,1), (1,0), (0,-1), (-1,0)]
            forM_ envPoss $ \envPos -> writeArray locs envPos locBorder
            return $ PQ.fromList $
               map (\envPos -> (diffs!envPos, ((locs, diffs), envPos))) envPoss

   forM masks $
      fmap (amap (Bool8.fromBool . (/=locOutside))) . CArrayPriv.freezeIOCArray
