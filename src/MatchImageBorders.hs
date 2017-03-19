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

import KneadShape (Vec2(Vec2), Dim2)

import qualified Data.Array.Knead.Simple.Physical as Phys

import qualified Data.PQueue.Prio.Max as PQ
import qualified Data.Set as Set
import Data.PQueue.Prio.Max (MaxPQueue)
import Data.Set (Set)

import qualified Data.Array.CArray.Base as CArrayPriv
import Data.Array.IOCArray (IOCArray)
import Data.Array.MArray (readArray, writeArray, freeze, thaw)
import Data.Array.CArray (CArray)
import Data.Array.IArray
         (Ix, amap, bounds, range, rangeSize, inRange, (!), (//))

import Foreign.Storable (Storable)

import Data.Traversable (forM)
import Data.Tuple.HT (mapSnd)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word8)

import Control.Monad (filterM)
import Control.Applicative ((<$>))


arrayCFromKnead :: Phys.Array Dim2 a -> IO (CArray (Int,Int) a)
arrayCFromKnead (Phys.Array (Vec2 height width) fptr) =
   CArrayPriv.unsafeForeignPtrToCArray fptr
      ((0,0), (fromIntegral height - 1, fromIntegral width - 1))

arrayKneadFromC ::
   (Storable a) => CArray (Int,Int) a -> Phys.Array Dim2 a
arrayKneadFromC carray =
   case bounds carray of
      ((ly,lx), (uy,ux)) ->
         Phys.Array
            (Vec2
               (fromIntegral (rangeSize (ly,uy)))
               (fromIntegral (rangeSize (lx,ux))))
            (snd $ CArrayPriv.toForeignPtr carray)


findBorder :: (Ix i, Enum i, Ix j, Enum j) => CArray (i,j) Bool -> Set (i,j)
findBorder mask =
   let ((yl,xl), (yu,xu)) = bounds mask
       revRange (l,u) = [u, pred u .. l]
       findLeft y =
         listToMaybe $ dropWhile (\x -> not $ mask!(y,x)) $ range (xl,xu)
       findRight y =
         listToMaybe $ dropWhile (\x -> not $ mask!(y,x)) $ revRange (xl,xu)
       findTop x =
         listToMaybe $ dropWhile (\y -> not $ mask!(y,x)) $ range (yl,yu)
       findBottom x =
         listToMaybe $ dropWhile (\y -> not $ mask!(y,x)) $ revRange (yl,yu)
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

prepareLocations :: (Ix ix) => CArray ix Bool -> Set ix -> CArray ix Location
prepareLocations mask border =
   amap (\b -> if b then locInside else locOutside) mask
   //
   map (flip (,) locBorder) (Set.toList border)

prepareShaping ::
   (Ix i, Enum i, Ix j, Enum j) =>
   [(CArray (i,j) Bool, CArray (i,j) Float)] ->
   IO ([IOCArray (i,j) Location],
       MaxPQueue Float ((IOCArray (i,j) Location, CArray (i,j) Float), (i,j)))
prepareShaping maskWeightss =
   fmap (mapSnd PQ.unions . unzip) $
   forM maskWeightss $ \(mask, weights) -> do
      let border = findBorder mask
      locations <- thaw $ prepareLocations mask border
      return
         (locations,
          fmap ((,) (locations, weights)) $ pqueueFromBorder weights border)

shapeParts ::
   IOCArray (Int, Int) Int ->
   [IOCArray (Int, Int) Location] ->
   MaxPQueue Float
      ((IOCArray (Int, Int) Location, CArray (Int, Int) Float), (Int, Int)) ->
   IO [CArray (Int, Int) Bool]
shapeParts count masks =
   let loop queue =
         case PQ.maxView queue of
            Nothing -> mapM (fmap (amap (/=locOutside)) . freeze) masks
            Just (((locs, diffs), pos@(y,x)), remQueue) -> do
               n <- readArray count pos
               if n<=1
                 then loop remQueue
                 else do
                     writeArray count pos (n-1)
                     writeArray locs pos locOutside
                     envPoss <-
                        filterM (fmap (locInside ==) . readArray locs) $
                        filter (inRange (bounds diffs)) $
                        map
                           (\(dy,dx) -> (y+dy, x+dx))
                           [(0,1), (1,0), (0,-1), (-1,0)]
                     mapM_
                        (\envPos -> writeArray locs envPos locBorder)
                        envPoss
                     loop $ PQ.union remQueue $ PQ.fromList $
                        map (\envPos -> (diffs!envPos, ((locs, diffs), envPos))) envPoss
   in  loop
