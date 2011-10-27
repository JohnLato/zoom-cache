{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.Dump
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Reading of ZoomCache files.
----------------------------------------------------------------------

module Data.ZoomCache.Dump (
    -- * Functions
      zoomDumpFile
    , zoomDumpSummary
    , zoomDumpSummaryLevel
    , zoomInfoFile
) where

import Control.Applicative ((<$>))
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import Text.Printf

import Data.ZoomCache

------------------------------------------------------------

zoomInfoFile :: FilePath -> IO ()
zoomInfoFile path = I.fileDriverRandom iterHeaders path >>= info

zoomDumpFile :: TrackNo -> FilePath -> IO ()
zoomDumpFile trackNo = I.fileDriverRandom (mapStream (dumpData trackNo))

zoomDumpSummary :: TrackNo -> FilePath -> IO ()
zoomDumpSummary trackNo = I.fileDriverRandom (mapStream (dumpSummary trackNo))

zoomDumpSummaryLevel :: TrackNo -> Int -> FilePath -> IO ()
zoomDumpSummaryLevel trackNo lvl = I.fileDriverRandom (mapStream (dumpSummaryLevel trackNo lvl))

----------------------------------------------------------------------

info :: CacheFile -> IO ()
info CacheFile{..} = do
    putStrLn . prettyGlobal $ cfGlobal
    mapM_ (putStrLn . uncurry prettyTrackSpec) . IM.assocs $ cfSpecs

streamRate :: Stream -> Maybe Rational
streamRate StreamNull = Nothing
streamRate s          = specRate <$> IM.lookup (strmTrack s) (cfSpecs (strmFile s))

dumpData :: TrackNo -> Stream -> IO ()
dumpData trackNo s@StreamPacket{..}
    | strmTrack == trackNo = mapM_ (\(t,d) -> printf "%s: %s\n" t d) tds
    | otherwise            = return ()
    where
        pretty = case streamRate s of
            Just r  -> prettyTimeStamp r
            Nothing -> show . unTS
        tds = zip (map pretty (packetTimeStamps strmPacket)) vals
        vals = f (packetData strmPacket)
        f (ZoomRaw a) = map prettyRawData a
dumpData _ _ = return ()

dumpSummary :: TrackNo -> Stream -> IO ()
dumpSummary trackNo s@StreamSummary{..}
    | strmTrack == trackNo = case streamRate s of
        Just r  -> putStrLn $ f r strmSummary
        Nothing -> return ()
    | otherwise            = return ()
    where
        f r (ZoomSummary a) = prettySummary r a
dumpSummary _ _           = return ()

dumpSummaryLevel :: TrackNo -> Int -> Stream -> IO ()
dumpSummaryLevel trackNo level s@StreamSummary{..}
    | level == opLevel strmSummary && strmTrack == trackNo = dumpSummary trackNo s
    | otherwise                                            = return ()
    where opLevel (ZoomSummary a) = summaryLevel a
dumpSummaryLevel _ _ _ = return ()

