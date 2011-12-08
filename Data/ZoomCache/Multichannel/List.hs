{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Multichannel.List
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for multichannel values of type [a].

This module implements the interfaces documented in "Data.ZoomCache.Codec".

Multichannel SummaryData is simply a concatenation of n blocks of SummaryData
for type a.

-}
----------------------------------------------------------------------

module Data.ZoomCache.Multichannel.List (
      SummaryData(..)
    , SummaryWork(..)
    , supportMultichannel
    , identifyCodecMultichannel
    , oneTrackMultichannel
    , mkTrackSpecMultichannel
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Functor.Identity
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import Data.Iteratee (Iteratee)
import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.TypeLevel.Num hiding ((==))

import Data.ZoomCache.Codec
import Data.ZoomCache.Common
import Data.ZoomCache.NList
import Data.ZoomCache.Types
import Data.Iteratee.ZoomCache.Utils

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeNList :: ByteString
trackTypeNList = "ZOOMmchn"

----------------------------------------------------------------------

supportMultichannel :: [IdentifyCodec] -> [IdentifyCodec]
supportMultichannel = f
    where f x = x ++ [identifyCodecMultichannel (f x)]

runner1 :: Identity (I.Iteratee s Identity c) -> c
runner1 = runIdentity . I.run . runIdentity

identifyCodecMultichannel :: [IdentifyCodec] -> IdentifyCodec
identifyCodecMultichannel identifiers bs = runner1 $ I.enumPure1Chunk bs identifyMulti
    where
        identifyMulti :: (Functor m, Monad m) => I.Iteratee ByteString m (Maybe Codec)
        identifyMulti = do
            mIdent <- B.pack <$> (I.joinI $ I.takeUpTo 8 I.stream2list)
            if mIdent == trackTypeNList
                then do
                    channels <- readInt32be
                    subIdentLength <- readInt32be
                    subCodec <- readCodec identifiers subIdentLength
                    return (fmap (foo channels) subCodec)
                else return Nothing

        foo :: Int -> Codec -> Codec
        foo channels (Codec a) = reifyIntegral channels (\n -> Codec (NList n [a]))

----------------------------------------------------------------------

-- | Create a track map for a stream of a given type, as track no. 1
oneTrackMultichannel :: (ZoomReadable a)
                     => Int -> a -> Bool -> Bool -> SampleRateType -> Rational -> ByteString -> TrackMap
oneTrackMultichannel channels a delta zlib !drType !rate !name =
    IM.singleton 1 (mkTrackSpecMultichannel channels a delta zlib drType rate name)
{-# INLINABLE oneTrackMultichannel #-}

mkTrackSpecMultichannel :: (ZoomReadable a)
                        => Int -> a -> Bool -> Bool -> SampleRateType -> Rational -> ByteString
                        -> TrackSpec
mkTrackSpecMultichannel channels a = reifyIntegral channels
    (\n -> TrackSpec (Codec (NList n [a])))

----------------------------------------------------------------------

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite [a] where
    write = writeList

writeList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> [a] -> ZoomW ()
writeList tn xs = reifyIntegral (length xs) (\n -> write tn (NList n xs))

instance (ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, [a]) where
    write = writeSOList

writeSOList :: (ZoomWrite a, ZoomWritable a) => TrackNo -> (SampleOffset, [a]) -> ZoomW ()
writeSOList tn (ts, xs) = reifyIntegral (length xs) (\n -> write tn (ts, (NList n xs)))

----------------------------------------------------------------------
-- Read

instance (Nat n, ZoomReadable a) => ZoomReadable (NList n a) where
    data SummaryData (NList n a) = SummaryNList (NList n (SummaryData a))

    trackIdentifier = mkTrackTypeNList

    readRaw     = readNList
    readSummary = readSummaryNList

    prettyRaw         = prettyPacketNList
    prettySummaryData = prettySummaryNList

mkTrackTypeNList :: (Nat n, ZoomReadable a) => (NList n a) -> ByteString
mkTrackTypeNList (NList nv l) = mconcat
    [ trackTypeNList
    , toByteString . fromIntegral32be . toInt $ nv
    , toByteString . fromIntegral32be . B.length $ subIdent
    , subIdent
    ]
    where
        subIdent = trackIdentifier (head l)

prettyPacketNList :: (Nat n, ZoomReadable a) => NList n a -> String
prettyPacketNList (NList _ l) = "[" ++ (concat $ intersperse "," (map prettyRaw l)) ++ "]"

readNList :: (Functor m, Monad m, Nat n, ZoomReadable a)
          => Iteratee ByteString m (NList n a)
readNList = NList unify <$> replicateM (toInt unify) readRaw
    where
        unify = undefined

readSummaryNList :: (Functor m, Monad m, Nat n, ZoomReadable a)
                 => Iteratee ByteString m (SummaryData (NList n a))
readSummaryNList = SummaryNList .
    NList unify <$> replicateM (toInt unify) readSummary
    where
        unify = undefined

prettySummaryNList :: (Nat n, ZoomReadable a) => SummaryData (NList n a) -> String
prettySummaryNList (SummaryNList (NList _ l)) =
    "[" ++ (concat $ intersperse "," (map prettySummaryData l)) ++ "]"

----------------------------------------------------------------------
-- Write

instance (Nat n, ZoomWrite a, ZoomWritable a) => ZoomWrite (NList n a) where
    write = writeData

instance (Nat n, ZoomWrite a, ZoomWritable a) => ZoomWrite (SampleOffset, (NList n a)) where
    write = writeDataVBR

instance (Nat n, ZoomWritable a) => ZoomWritable (NList n a) where
    data SummaryWork (NList n a) = SummaryWorkNList (NList n (SummaryWork a))

    fromRaw           = fromNList
    fromSummaryData   = fromSummaryNList

    initSummaryWork   = initSummaryNList
    toSummaryData     = mkSummaryNList
    updateSummaryData = updateSummaryNList
    appendSummaryData = appendSummaryNList

fromNList :: ZoomWritable a => (NList n a) -> Builder
fromNList (NList _ l) = mconcat $ map fromRaw l

initSummaryNList :: (Nat n, ZoomWritable a)
                 => SampleOffset -> SummaryWork (NList n a)
initSummaryNList entry = SummaryWorkNList $
    NList unify (replicate (toInt unify) (initSummaryWork entry))
    where
        unify = undefined

mkSummaryNList :: ZoomWritable a
               => SampleOffsetDiff -> SummaryWork (NList n a) -> SummaryData (NList n a)
mkSummaryNList dur (SummaryWorkNList (NList _ l)) =
    SummaryNList (NList undefined (map (toSummaryData dur) l))

fromSummaryNList :: ZoomWritable a
                 => SummaryData (NList n a) -> Builder
fromSummaryNList (SummaryNList (NList _ l)) = mconcat $ map fromSummaryData l

updateSummaryNList :: ZoomWritable a
                   => SampleOffset  -> (NList n a) -> SummaryWork (NList n a)
                   -> SummaryWork (NList n a)
updateSummaryNList t (NList _ xs) (SummaryWorkNList (NList _ ws)) =
    SummaryWorkNList (NList undefined (zipWith (updateSummaryData t) xs ws))

appendSummaryNList :: ZoomWritable a
                   => SampleOffsetDiff -> SummaryData (NList n a)
                   -> SampleOffsetDiff -> SummaryData (NList n a)
                   -> SummaryData (NList n a)
appendSummaryNList dur1 (SummaryNList (NList _ l1)) dur2 (SummaryNList (NList _ l2)) =
    SummaryNList (NList undefined (zipWith a l1 l2))
    where
        a s1 s2 = appendSummaryData dur1 s1 dur2 s2