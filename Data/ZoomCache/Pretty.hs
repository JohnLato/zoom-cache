{-# LANGUAGE RecordWildCards #-}
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
-- Pretty-printing of zoom-cache types
----------------------------------------------------------------------

module Data.ZoomCache.Pretty (
      prettyGlobal
    , prettyTrackSpec
    , prettySampleOffset
    , prettySummary
) where

import qualified Data.ByteString.Char8 as C
import Data.Ratio
import Text.Printf

import Data.ZoomCache.Common
import Data.ZoomCache.Types

----------------------------------------------------------------------

-- | Pretty-print a 'Global'
prettyGlobal :: Global -> String
prettyGlobal Global{..} = unlines
    [ "Version:\t\t" ++ show vMaj ++ "." ++ show vMin
    , "No. tracks:\t\t" ++ show noTracks
    , "Presentation-time:\t" ++ ratShow presentationTime
    , "Base-time:\t\t" ++ ratShow baseTime
    , "UTC baseTime:\t\t" ++ maybe "undefined" show baseUTC
    ]
    where
        Version vMaj vMin = version

-- | Pretty-print a 'TrackSpec'
prettyTrackSpec :: TrackNo -> TrackSpec -> String
prettyTrackSpec trackNo TrackSpec{..} = unlines
    [ "Track " ++ show trackNo ++ ":"
    , "\tName:\t" ++ C.unpack specName
    , "\tType:\t" ++ show specType
    , "\tEnc:\t"  ++ unwords [encoding, compression]
    , "\tRate:\t" ++ show specSRType ++ " " ++ ratShow specRate
    ]
    where
        encoding | specDeltaEncode = "Delta"
                 | otherwise       = "Raw"
        compression | specZlibCompress = "Zlib"
                    | otherwise        = "Uncompressed"

-- | Pretty-print a 'SampleOffset', given a datarate
prettySampleOffset :: Rational -> SampleOffset -> String
prettySampleOffset r (SO t)
    | d == 0    = "00:00:00.000"
    {-
    | d < 100   = printf "%02d:%02d:%02d::%02d" hrs minN secN framesN
    -}
    | otherwise = printf "%02d:%02d:%02d.%03d" hrs minN secN msN
    where
          d = denominator r
          n = numerator r
          msN = quot (1000 * framesN) n
          (secT, framesN) = quotRem (fromIntegral t*d) n
          (minT, secN) = quotRem secT 60
          (hrs, minN) = quotRem minT 60

-- | Pretty-print a 'Summary', given a datarate
prettySummary :: ZoomReadable a => Rational -> Summary a -> String
prettySummary r s = concat
    [ prettySummaryTimes r s
    , prettySummaryLevel s
    , prettySummaryData (summaryData s)
    ]

prettySummaryTimes :: Rational -> Summary a -> String
prettySummaryTimes r s = concat
    [ "[", (prettySampleOffset r $ summaryEntrySO s)
    , "-", (prettySampleOffset r $ summaryExitSO s), "] "
    ]

prettySummaryLevel :: Summary a -> String
prettySummaryLevel s = printf "lvl: %d" (summaryLevel s)

----------------------------------------------------------------------

ratShow :: Rational -> String
ratShow r
    | d == 0 = "0"
    | d == 1 = show n
    | otherwise = show n ++ "/" ++ show d
    where
        n = numerator r
        d = denominator r

