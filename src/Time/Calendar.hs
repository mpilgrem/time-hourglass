{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Time.Calendar
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Miscellaneous calendar functions.
-}

module Time.Calendar
  ( isLeapYear
  , getWeekDay
  , getDayOfTheYear
  , daysInMonth
  , dateToUnixEpoch
  , dateFromUnixEpoch
  , todToSeconds
  , dateTimeToUnixEpoch
  , dateTimeFromUnixEpoch
  , dateTimeFromUnixEpochP
  ) where

import           Time.Internal
                   ( dateTimeFromUnixEpoch, dateTimeFromUnixEpochP )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), Month (..)
                   , Seconds (..), TimeInterval (..), TimeOfDay (..), WeekDay
                   )

-- | For the given year in the Gregorian calendar, is it a leap year (366 days
-- long)?
isLeapYear ::
     Int
     -- ^ Year.
  -> Bool
isLeapYear year
  | year `mod` 4 /= 0   = False
  | year `mod` 100 /= 0 = True
  | year `mod` 400 == 0 = True
  | otherwise           = False

-- | For the given date in the proleptic Gregorian calendar, yield the day of
-- the week it falls on.
getWeekDay :: Date -> WeekDay
getWeekDay date = toEnum (d `mod` 7)
 where
  d = daysOfDate date

-- | For the given year and month in the proleptic Gregorian calendar, yield the
-- number of days from the start of the year to the start of the month.
daysUntilMonth ::
     Int
     -- ^ Year.
  -> Month
  -> Int
daysUntilMonth y m
  | isLeapYear y = leapYears !! fromEnum m
  | otherwise    = normalYears !! fromEnum m
 where
  normalYears = [ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ]
  leapYears   = [ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ]

-- | For the given year and month in the proleptic Gregorian calendar, yield the
-- number of days in the month.
daysInMonth ::
     Int
     -- ^ Year.
  -> Month
  -> Int
daysInMonth y m
  | m == February && isLeapYear y = 29
  | otherwise                     = days !! fromEnum m
 where
  days = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

-- | For the given date in the proleptic Gregorian calendar, yield the number of
-- days before the date in the same year. For example, there are @0@ days before
-- 1st January.
getDayOfTheYear :: Date -> Int
getDayOfTheYear (Date y m d) = daysUntilMonth y m + d

-- | For the given year in the proleptic Gregorian calendar, yield the number of
-- days before 1st January of the year and since 1st January 1 CE.
daysBeforeYear :: Int -> Int
daysBeforeYear year = y * 365 + (y `div` 4) - (y `div` 100) + (y `div` 400)
 where
  y = year - 1

-- | For the given date in the proleptic Gregorian calendar, yield the number of
-- days since 1st January 1 CE.
daysOfDate :: Date -> Int
daysOfDate (Date y m d) = daysBeforeYear y + daysUntilMonth y m + d

-- | For the given date in the proleptic Gregorian calendar, and assuming a time
-- of 00:00:00 UTC, yield the number of non-leap seconds since the Unix epoch
-- (1970-01-01 00:00:00 UTC).
dateToUnixEpoch :: Date -> Elapsed
dateToUnixEpoch date =
  Elapsed $ Seconds (fromIntegral (daysOfDate date - epochDays) * secondsPerDay)
 where
  epochDays     = 719_163
  secondsPerDay = 86_400 -- Julian day is 24h

-- | For the given period of time since the Unix epoch
-- (1970-01-01 00:00:00 UTC), yield the corresponding date in the proleptic
-- Gregorian calendar.
dateFromUnixEpoch :: Elapsed -> Date
dateFromUnixEpoch e = dtDate $ dateTimeFromUnixEpoch e

-- | For the given time of day, yield the number of seconds since the start of
-- the day.
todToSeconds :: TimeOfDay -> Seconds
todToSeconds (TimeOfDay h m s _) = toSeconds h + toSeconds m + s

-- | For the given date (in the proleptic Gregorian calendar) and time (in UTC),
-- yield the number of non-leap seconds that have elapsed since the Unix epoch
-- (1970-01-01 00:00:00 UTC).
dateTimeToUnixEpoch :: DateTime -> Elapsed
dateTimeToUnixEpoch (DateTime d t) =
  dateToUnixEpoch d + Elapsed (todToSeconds t)
