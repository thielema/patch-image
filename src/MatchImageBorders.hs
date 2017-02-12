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
import Data.PQueue.Prio.Max (MaxPQueue)

import Data.Array.IOCArray (IOCArray)
import Data.Array.MArray (readArray, writeArray, freeze)
import Data.Array.CArray (CArray)
import Data.Array.IArray (amap, bounds, inRange, (!))
import Data.Word (Word8)

import Control.Monad (filterM)


type Location = Word8

locOutside, locBorder, locInside :: Location
locOutside = 0
locBorder = 1
locInside = 2

shapeParts ::
   [IOCArray (Int, Int) Location] ->
   IOCArray (Int, Int) Int ->
   MaxPQueue Float
      ((IOCArray (Int, Int) Location, CArray (Int, Int) Float), (Int, Int)) ->
   IO [CArray (Int,Int) Bool]
shapeParts masks count =
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
                           [(0,1), (1,1), (1,0), (1,-1),
                            (0,-1), (-1,-1), (-1,0), (-1,1)]
                     mapM_
                        (\envPos -> writeArray locs envPos locBorder)
                        envPoss
                     loop $ PQ.union remQueue $ PQ.fromList $
                        map (\envPos -> (diffs!envPos, ((locs, diffs), envPos))) envPoss
   in  loop
