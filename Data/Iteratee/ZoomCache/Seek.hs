{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.Iteratee.ZoomCache.Seek
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Iteratee reading of ZoomCache files.
----------------------------------------------------------------------

module Data.Iteratee.ZoomCache.Seek (
    -- * Seeking
      seekTimeStamp
    , seekUTCTime
) where

import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Time.Clock (UTCTime)
import System.Posix.Types (FileOffset)

import Data.ZoomCache.Common
import Data.ZoomCache.Types

----------------------------------------------------------------------

seekTimeStamp :: (LL.ListLike s el, Timestampable el, Monad m)
              => CacheFile -> Maybe TimeStamp -> Iteratee s m ()
seekTimeStamp _  Nothing   = return ()
seekTimeStamp cf (Just ts) = do
    I.seek (nearOffset cf ts)
    dropWhileB (before (Just ts))

seekUTCTime :: (LL.ListLike s el, UTCTimestampable el, Monad m)
            => Maybe UTCTime -> Iteratee s m ()
seekUTCTime uts = do
    I.seek 0
    dropWhileB (beforeUTC uts)

nearOffset :: CacheFile -> TimeStamp -> FileOffset
nearOffset cf (TS ts)
    | IM.null earlier = 0
    | otherwise       = snd (IM.findMax earlier)
    where
        earlier = fst $ IM.split ms (cfOffsets cf)
        ms :: Int
        ms = floor . (* 1000.0) $ ts

-- |Skip all elements while the predicate is true, but also return the last true element
dropWhileB :: (Monad m, LL.ListLike s el) => (el -> Bool) -> I.Iteratee s m ()
dropWhileB p = I.icontP step
  where
    step I.NoData    = I.continueP step
    step (I.Chunk str)
      | LL.null left = I.continueP step
      | otherwise    = I.ContDone () (I.Chunk left)
      where
        left = llDropWhileB p str
    step s@I.EOF{}   = I.ContDone () s
{-# INLINE dropWhileB #-}

{- | Drops all elements from the start of the list that satisfy the
       function, except for the last one. -}
llDropWhileB :: LL.ListLike full item => (item -> Bool) -> full -> full
llDropWhileB = dw LL.empty
    where
        dw prev func l
            | LL.null l = prev
            | func (LL.head l) = dw (LL.take 1 l) func (LL.tail l)
            | otherwise = LL.append prev l

