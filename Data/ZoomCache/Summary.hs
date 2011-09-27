{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Summary (
  -- * Types
    Summary(..)
  , summaryDuration
  , appendSummary
) where

import Data.ZoomCache.Common

data Summary = Summary
    { summaryTrack :: ZoomTrackNo
    , summaryLevel :: Int
    , summaryEntryTime :: Int
    , summaryExitTime :: Int
    , summaryEntry :: Double
    , summaryExit :: Double
    , summaryMin :: Double
    , summaryMax :: Double
    , summaryAvg :: Double
    , summaryRMS :: Double
    }

summaryDuration :: Summary -> Int
summaryDuration Summary{..} = summaryExitTime - summaryEntryTime

-- XXX: summaries are only compatible if tracks and levels are equal
appendSummary :: Summary -> Summary -> Summary
appendSummary s1 s2 = Summary
    { summaryTrack = summaryTrack s1
    , summaryLevel = summaryLevel s1
    , summaryEntryTime = summaryEntryTime s1
    , summaryExitTime = summaryExitTime s2
    , summaryEntry = summaryEntry s1
    , summaryExit = summaryExit s2
    , summaryMin = min (summaryMin s1) (summaryMin s2)
    , summaryMax = max (summaryMax s1) (summaryMax s2)
    , summaryAvg = ((summaryAvg s1 * dur s1) +
                    (summaryAvg s2 * dur s2)) /
                   (dur s1 + dur s2)
    , summaryRMS = sqrt $ ((summaryRMS s1 * summaryRMS s1 * dur s1) +
                           (summaryRMS s2 * summaryRMS s2 * dur s2)) /
                          (dur s1 + dur s2)
    }
    where
        dur = fromIntegral . summaryDuration