{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Write
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Writing of ZoomCache files.
----------------------------------------------------------------------

module Data.ZoomCache.Write (
    -- * The ZoomWrite class
      ZoomWrite(..)

    -- * Instance helpers
    , writeData
    , writeDataVBR

    -- * The ZoomW monad
    , ZoomW
    , withFileWrite
    , flush

    -- * ZoomWHandle IO functions
    , ZoomWHandle
    , openWrite

    -- * Watermarks
    , watermark
    , setWatermark

    -- * TrackSpec helpers
    , oneTrack
) where

import Blaze.ByteString.Builder hiding (flush)
import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Foldable as Fold
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Typeable
import System.IO

import Data.ZoomCache.Binary
import Data.ZoomCache.Common
import Data.ZoomCache.Types

------------------------------------------------------------

-- | The ZoomWrite class provides 'write', a method to write a
-- Haskell value to an open ZoomCache file.
--
class ZoomWrite t where
    -- | Write a value to an open ZoomCache file.
    write :: TrackNo -> t -> ZoomW ()

instance ZoomWrite Double where
    write = writeData

instance ZoomWrite (TimeStamp, Double) where
    write = writeDataVBR

------------------------------------------------------------

data ZoomWHandle = ZoomWHandle
    { whHandle    :: Handle
    , whTrackWork :: IntMap TrackWork
    , whDeferred  :: IntMap Builder
    , whWriteData :: Bool
    }

data TrackWork = TrackWork
    { twSpec      :: TrackSpec
    , twBuilder   :: Builder
    , twTSBuilder :: Builder
    , twCount     :: Int
    , twWatermark :: Int
    , twEntryTime :: TimeStamp
    , twExitTime  :: TimeStamp
    , twWriter    :: Maybe OpaqueSummaryWrite
    }

----------------------------------------------------------------------
-- Public API

-- | A StateT IO monad for writing a ZoomCache file
type ZoomW = StateT ZoomWHandle IO

-- | Run a @ZoomW ()@ action on a given file handle, using the specified
-- 'TrackMap' specification
withFileWrite :: TrackMap
              -> Bool          -- ^ Whether or not to write raw data packets.
                               -- If False, only summary blocks are written.
              -> ZoomW ()
              -> FilePath
              -> IO ()
withFileWrite ztypes doRaw f path = do
    z <- openWrite ztypes doRaw path
    z' <- execStateT (f >> flush) z
    hClose (whHandle z')

-- | Force a flush of ZoomCache summary blocks to disk. It is not usually
-- necessary to call this function as summary blocks are transparently written
-- at regular intervals.
flush :: ZoomW ()
flush = do
    h <- gets whHandle
    tracks <- gets whTrackWork
    doRaw <- gets whWriteData
    when doRaw $
        liftIO $ Fold.mapM_ (L.hPut h) $ IM.mapWithKey bsFromTrack tracks
    mapM_ (uncurry flushSummary) (IM.assocs tracks)
    pending <- mconcat . IM.elems <$> gets whDeferred
    liftIO . L.hPut h . toLazyByteString $ pending
    modify $ \z -> z
        { whTrackWork = IM.map flushTrack (whTrackWork z)
        , whDeferred = IM.empty
        }
    where
        flushTrack :: TrackWork -> TrackWork
        flushTrack tw = d{twWriter = clearWork <$> (twWriter tw)}
            where
                d = mkTrackWork (twSpec tw) (twExitTime tw) (twWatermark tw)

-- | Open a new ZoomCache file for writing, using a specified 'TrackMap'.
openWrite :: TrackMap
          -> Bool              -- ^ Whether or not to write raw data packets.
                               -- If False, only summary blocks are written.
          -> FilePath
          -> IO ZoomWHandle
openWrite trackMap doRaw path = do
    h <- openFile path WriteMode
    let global = mkGlobal (IM.size trackMap)
    writeGlobalHeader h global
    let tracks = IM.foldWithKey addTrack IM.empty trackMap
    mapM_ (uncurry (writeTrackHeader h)) (IM.assocs trackMap)
    return $ ZoomWHandle h tracks IM.empty doRaw
    where
        addTrack :: TrackNo -> TrackSpec
                 -> IntMap TrackWork
                 -> IntMap TrackWork
        addTrack trackNo spec = IM.insert trackNo trackState
            where
                trackState = mkTrackWork spec (TS 0) 1024

-- | Create a track map for a stream of a given type, as track no. 1
oneTrack :: TrackType -> DataRateType -> Rational -> L.ByteString -> TrackMap
oneTrack zType drType rate name = IM.singleton 1 (TrackSpec zType drType rate name)

-- | Query the maximum number of data points to buffer for a given track before
-- forcing a flush of all buffered data and summaries.
watermark :: TrackNo -> ZoomW (Maybe Int)
watermark trackNo =  do
    track <- IM.lookup trackNo <$> gets whTrackWork
    return (twWatermark <$> track)

-- | Set the maximum number of data points to buffer for a given track before
-- forcing a flush of all buffered data and summaries.
setWatermark :: TrackNo -> Int -> ZoomW ()
setWatermark trackNo w = modifyTrack trackNo f
    where
        f :: TrackWork -> TrackWork
        f tw = tw { twWatermark = w }

----------------------------------------------------------------------
-- Global header

writeGlobalHeader :: Handle -> Global -> IO ()
writeGlobalHeader h = L.hPut h . toLazyByteString . fromGlobal

----------------------------------------------------------------------
-- Track header

writeTrackHeader :: Handle -> Int -> TrackSpec -> IO ()
writeTrackHeader h trackNo TrackSpec{..} = do
    L.hPut h . mconcat $
        [ trackHeader
        , toLazyByteString $ mconcat
            [ fromTrackNo trackNo
            , fromTrackType specType
            , fromDataRateType specDRType
            , fromRational64 specRate
            , encInt . LC.length $ specName
            ]
        , specName
        ]

----------------------------------------------------------------------
-- Data

incTimeStamp :: TimeStamp -> TimeStamp
incTimeStamp (TS t) = TS (t+1)

incTime :: TrackNo -> ZoomW ()
incTime trackNo = modifyTrack trackNo $ \tw -> tw
    { twEntryTime = if twCount tw == 0
                        then (incTimeStamp (twEntryTime tw))
                        else twEntryTime tw
    , twExitTime = incTimeStamp (twExitTime tw)
    }

setTime :: TrackNo -> TimeStamp -> ZoomW ()
setTime trackNo t = modifyTrack trackNo $ \tw -> tw
    { twEntryTime = if twCount tw == 0 then t else twEntryTime tw
    , twExitTime = t
    }

flushIfNeeded :: TrackNo -> ZoomW ()
flushIfNeeded trackNo = do
    zt <- IM.lookup trackNo <$> gets whTrackWork
    case zt of
        Just track -> when (flushNeeded track) flush
        Nothing -> error "no such track" -- addTrack trackNo, if no data has been written
    where
        flushNeeded :: TrackWork -> Bool
        flushNeeded TrackWork{..} = twCount >= twWatermark

writeData :: (Typeable a, ZoomWrite a, ZoomSummaryWrite a)
          => TrackNo -> a -> ZoomW ()
writeData trackNo d = do
    incTime trackNo

    doRaw <- gets whWriteData
    when doRaw $
        modifyTrack trackNo $ \z -> z { twBuilder = twBuilder z <> builder d }

    modifyTrack trackNo $ \z -> z
        { twCount = twCount z + 1
        , twWriter = updateOpSumm (twCount z) (twExitTime z) d (twWriter z)
        }
    flushIfNeeded trackNo

writeDataVBR :: (Typeable a, ZoomWrite a, ZoomSummaryWrite a)
             => TrackNo -> (TimeStamp, a) -> ZoomW ()
writeDataVBR trackNo (t, d) = do
    setTime trackNo t

    doRaw <- gets whWriteData
    when doRaw $
        modifyTrack trackNo $ \z -> z
            { twBuilder = twBuilder z <> builder d
            , twTSBuilder = twTSBuilder z <>
                  (encInt64 .  unTS) t
            }

    modifyTrack trackNo $ \z -> z
        { twCount = twCount z + 1
        , twWriter = updateOpSumm (twCount z) t d (twWriter z)
        }
    flushIfNeeded trackNo

----------------------------------------------------------------------
-- Global

mkGlobal :: Int -> Global
mkGlobal n = Global
    { version = Version versionMajor versionMinor
    , noTracks = n
    , presentationTime = 0
    , baseTime = 0
    , baseUTC = Nothing
    }

----------------------------------------------------------------------
-- TrackState

modifyTracks :: (IntMap TrackWork -> IntMap TrackWork) -> ZoomW ()
modifyTracks f = modify (\z -> z { whTrackWork = f (whTrackWork z) })

modifyTrack :: TrackNo -> (TrackWork -> TrackWork) -> ZoomW ()
modifyTrack trackNo f = modifyTracks (IM.adjust f trackNo)

bsFromTrack :: TrackNo -> TrackWork -> L.ByteString
bsFromTrack trackNo TrackWork{..} = toLazyByteString $ mconcat
    [ fromLazyByteString packetHeader
    , encInt trackNo
    , encInt64 . unTS $ twEntryTime
    , encInt64 . unTS $ twExitTime
    , encInt (len twBuilder + len twTSBuilder)
    , encInt twCount
    , twBuilder
    , twTSBuilder
    ]
    where
        len = L.length . toLazyByteString

mkTrackWork :: TrackSpec -> TimeStamp -> Int -> TrackWork
mkTrackWork spec entry w = TrackWork
        { twSpec = spec
        , twBuilder = mempty
        , twTSBuilder = mempty
        , twCount = 0
        , twWatermark = w
        , twEntryTime = entry
        , twExitTime = entry
        , twWriter = Nothing
        }

----------------------------------------------------------------------
-- Summary

flushSummary :: TrackNo -> TrackWork -> ZoomW ()
flushSummary trackNo TrackWork{..} = case twWriter of
    Just writer -> do
        let (writer', bs) = flushOpSumm trackNo twEntryTime twExitTime writer
        modify $ \z -> z { whDeferred = IM.unionWith mappend (whDeferred z) bs }
        modifyTrack trackNo (\ztt -> ztt { twWriter = Just writer' } )
    _           -> return ()

flushOpSumm :: TrackNo -> TimeStamp -> TimeStamp
            -> OpaqueSummaryWrite -> (OpaqueSummaryWrite, IntMap Builder)
flushOpSumm _       _         _        op@(OpSummaryWrite _ Nothing) = (op, IM.empty)
flushOpSumm trackNo entryTime exitTime (OpSummaryWrite l (Just cw))  =
    (OpSummaryWrite l' (Just cw), bs)
    where
        (bs, l') = pushSummary s IM.empty l
        s = Summary
            { summaryTrack = trackNo
            , summaryLevel = 1
            , summaryEntryTime = entryTime
            , summaryExitTime = exitTime
            , summaryData = mkSummaryData dur cw
            }
        dur = fromIntegral $ (unTS exitTime) - (unTS entryTime)

pushSummary :: (ZoomSummaryWrite a)
            => Summary a
            -> IntMap Builder -> IntMap (Summary a -> Summary a)
            -> (IntMap Builder, IntMap (Summary a -> Summary a))
pushSummary s bs l = do
    case IM.lookup (summaryLevel s) l of
        Just g  -> pushSummary (g s) bs' cleared
        Nothing -> (bs', inserted)
    where
        bs' = IM.insert (summaryLevel s) (fromSummary s) bs
        f next = (s `appendSummary` next) { summaryLevel = summaryLevel s + 1 }
        inserted = IM.insert (summaryLevel s) f l
        cleared = IM.delete (summaryLevel s) l

------------------------------------------------------------

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

