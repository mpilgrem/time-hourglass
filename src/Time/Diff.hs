{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module      : Time.Diff
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Time arithmetic methods.
-}

module Time.Diff
  ( Duration (..)
  , Period (..)
  , durationNormalize
  , durationFlatten
  , elapsedTimeAddSeconds
  , elapsedTimeAddSecondsP
  , dateAddPeriod
  ) where

import           Control.DeepSeq ( NFData (..) )
import           Data.Data ( Data )
import           Time.Calendar ( daysInMonth )
import           Time.Types
                   ( Date (..), Elapsed (..), ElapsedP (..), Hours (..)
                   , Minutes (..), NanoSeconds (..), Seconds (..)
                   , TimeInterval (..)
                   )

-- | Type representing periods of time in years, months and days.
-- See t'Duration' for periods of time hours, minutes, seconds and nanoseconds.
data Period = Period
  { periodYears  :: !Int
  , periodMonths :: !Int
  , periodDays   :: !Int
  }
  deriving (Data, Eq, Ord, Read, Show)

instance NFData Period where
  rnf (Period y m d) = y `seq` m `seq` d `seq` ()

instance Semigroup Period where
  (<>) (Period y1 m1 d1) (Period y2 m2 d2) =
    Period (y1 + y2) (m1 + m2) (d1 + d2)

instance Monoid Period where
  mempty = Period 0 0 0

-- | Type represeting periods of time in hours, minutes, seconds and
-- nanoseconds. See t'Period' for periods of time in years, months and days.
data Duration = Duration
  { durationHours   :: !Hours       -- ^ number of hours
  , durationMinutes :: !Minutes     -- ^ number of minutes
  , durationSeconds :: !Seconds     -- ^ number of seconds
  , durationNs      :: !NanoSeconds -- ^ number of nanoseconds
  }
  deriving (Data, Eq, Ord, Read, Show)

instance NFData Duration where
  rnf (Duration h m s ns) = h `seq` m `seq` s `seq` ns `seq` ()

instance Semigroup Duration where
  (<>) (Duration h1 m1 s1 ns1) (Duration h2 m2 s2 ns2) =
    Duration (h1 + h2) (m1 + m2) (s1 + s2) (ns1 + ns2)

instance Monoid Duration where
  mempty = Duration 0 0 0 0

instance TimeInterval Duration where
  fromSeconds s = (durationNormalize (Duration 0 0 s 0), 0)

  toSeconds d   = fst $ durationFlatten d

-- | Convert a t'Duration' to an equivalent number of seconds and nanoseconds.
durationFlatten :: Duration -> (Seconds, NanoSeconds)
durationFlatten (Duration h m s (NanoSeconds ns)) =
  (toSeconds h + toSeconds m + s + Seconds sacc, NanoSeconds ns')
 where
  (sacc, ns') = ns `divMod` 1000000000

-- | Normalize a t'Duration' to represent the same period of time with the
-- biggest units possible. For example, 62 minutes is normalized as
-- 1 hour and 2 minutes.
durationNormalize :: Duration -> Duration
durationNormalize (Duration (Hours h) (Minutes mi) (Seconds s) (NanoSeconds ns)) =
  Duration (Hours (h+hacc)) (Minutes mi') (Seconds s') (NanoSeconds ns')
 where
  (hacc, mi') = (mi+miacc) `divMod` 60
  (miacc, s') = (s+sacc) `divMod` 60
  (sacc, ns') = ns `divMod` 1000000000

-- | Add the given period of time to the given date.
dateAddPeriod :: Date -> Period -> Date
dateAddPeriod (Date yOrig mOrig dOrig) (Period yDiff mDiff dDiff) =
  loop (yOrig + yDiff + yDiffAcc) mStartPos (dOrig + dDiff)
 where
  (yDiffAcc,mStartPos) = (fromEnum mOrig + mDiff) `divMod` 12
  loop y m d
    | d <= 0 =
        let (m', y') = if m == 0
              then (11, y - 1)
              else (m - 1, y)
        in  loop y' m' (daysInMonth y' (toEnum m') + d)
    | d <= dMonth = Date y (toEnum m) d
    | otherwise =
        let newDiff = d - dMonth
        in  if m == 11
              then loop (y + 1) 0 newDiff
              else loop y (m + 1) newDiff
   where
    dMonth = daysInMonth y (toEnum m)

-- | Add the given number of seconds to the given t'Elapsed' value.
elapsedTimeAddSeconds :: Elapsed -> Seconds -> Elapsed
elapsedTimeAddSeconds (Elapsed s1) s2 = Elapsed (s1 + s2)

-- | Add the given number of seconds to the given t'ElapsedP' value.
elapsedTimeAddSecondsP :: ElapsedP -> Seconds -> ElapsedP
elapsedTimeAddSecondsP (ElapsedP (Elapsed s1) ns1) s2 =
  ElapsedP (Elapsed (s1 + s2)) ns1

{- disabled for warning purpose. to be implemented

-- | Duration string to time diff
--
-- <http://en.wikipedia.org/wiki/ISO_8601#Durations>
--
-- * P is the duration designator (historically called "period") placed at the start of the duration representation.
--
-- * Y is the year designator that follows the value for the number of years.
--
-- * M is the month designator that follows the value for the number of months.
--
-- * W is the week designator that follows the value for the number of weeks.
--
-- * D is the day designator that follows the value for the number of days.
--
-- * T is the time designator that precedes the time components of the representation.
--
-- * H is the hour designator that follows the value for the number of hours.
--
-- * M is the minute designator that follows the value for the number of minutes.
--
-- * S is the second designator that follows the value for the number of seconds.
--
timeDiffFromDuration :: String -> TimeDiff
timeDiffFromDuration _ = undefined

timeDiffFromString :: String -> (

-- | Human description string to time diff
--
-- examples:
--
-- * "1 day"
--
-- * "2 months, 5 days and 1 second"
--
timeDiffFromDescription :: String -> TimeDiff
timeDiffFromDescription _ = undefined
-}
