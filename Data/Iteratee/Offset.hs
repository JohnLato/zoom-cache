{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}

module Data.Iteratee.Offset (
      tell

    -- * Iteratee conversion
    , convOffset

    -- * ListLike
    , takeBS
) where

import Prelude hiding (drop, head)

import Control.Monad (liftM)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import System.Posix (FileOffset)

import Data.Offset

----------------------------------------------------------------------

tell :: (Monad m)
     => Iteratee (Offset ByteString) m FileOffset
tell = I.icontP step
  where
    step (I.NoData) = I.continueP step
    step s@(I.Chunk (Offset o vec))
      | LL.null vec = I.continueP step
      | otherwise   = I.ContDone o s
    step I.EOF{}    = I.continueErrP (I.EofException "zoom-cache: tell") step
{-# INLINE tell #-}

----------------------------------------------------------------------

-- | Run a ByteString iteratee on an (Offset ByteString) input stream
convOffset :: (Monad m)
           => Iteratee ByteString m a
           -> Iteratee (Offset ByteString) m a
convOffset = I.joinI . I.bimapChunks
    (\(Offset _ bs) -> bs)
    (\(Offset ofs orig) rest ->
       Offset (ofs+(fromIntegral $ LL.length orig-LL.length rest)) rest)
{-# INLINE convOffset #-}

----------------------------------------------------------------------

takeBS :: (Monad m)
       => Int -> Iteratee (Offset ByteString) m ByteString
takeBS n = (\(Offset _ bs) -> bs) `liftM` (I.joinI $ I.take n I.stream2stream)
