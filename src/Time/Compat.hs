{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Time.Compat
License     : BSD-style
Copyright   : (c) 2015 Nicolas DI PRIMA <nicolas@di-prima.fr>

Basic time conversion compatibility.

This module aims to help conversion between types from @time@ package and types
from the @time-hourglass@ package.

An example of use (taken from file examples/Example/Time/Compat.hs):

> import Data.Hourglass as H
> import Time.Compat as C
> import Data.Time as T
>
> transpose :: T.ZonedTime -> H.LocalTime H.DateTime
> transpose oldTime = H.localTime
>   offsetTime
>   (H.DateTime newDate timeofday)
>  where
>   T.ZonedTime (T.LocalTime day tod) (T.TimeZone tzmin _ _) = oldTime
>
>   newDate :: H.Date
>   newDate = C.dateFromMJDEpoch $ T.toModifiedJulianDay day
>
>   timeofday :: H.TimeOfDay
>   timeofday = C.diffTimeToTimeOfDay $ toRational $ T.timeOfDayToTime tod
>
>   offsetTime = H.TimezoneOffset $ fromIntegral tzmin
-}

module Time.Compat
  ( dateFromUnixEpoch
  , dateFromMJDEpoch
  , diffTimeToTimeOfDay
  ) where

import           Time.Time ( timeConvert )
import           Time.Types ( Date, Elapsed (..), TimeOfDay (..) )

-- | Given an integer which represents the number of days since the Unix epoch
-- (1970-01-01 00:00:00 UTC), yield the corresponding date in the
-- proleptic Gregorian calendar.
dateFromUnixEpoch ::
     Integer
     -- ^ Number of days since the Unix epoch (1970-01-01 00:00:00 UTC).
  -> Date
dateFromUnixEpoch day = do
  let sec = Elapsed $ fromIntegral $ day * 86_400
  timeConvert sec

-- | The number of days between the Modified Julian Date (MJD) epoch
-- (1858-11-17 00:00:00 UTC) and the Unix epoch (1970-01-01 00:00:00 UTC).
daysMJDtoUnix :: Integer
daysMJDtoUnix = 40_587

-- | Given an integer which represents the number of days since the Modified
-- Julian Date (MJD) epoch (1858-11-17 00:00:00 UTC), yields the corresponding
-- date in the proleptic Gregorian calendar.
--
-- This function allows a user to convert a t'Data.Time.Calendar.Day'
-- into t'Date'.
--
-- > import qualified Data.Time.Calendar as T
-- >
-- > timeDay :: T.Day
-- >
-- > dateFromMJDEpoch $ T.toModifiedJulianDay timeDay
dateFromMJDEpoch ::
     Integer
     -- ^ Number of days since 1858-11-17 00:00:00 UTC.
  -> Date
dateFromMJDEpoch dtai =
  dateFromUnixEpoch (dtai - daysMJDtoUnix)

-- | Given a real number representing the number of non-leap seconds since the
-- start of the day, yield a t'TimeOfDay' value (assuming no leap seconds).
--
-- Example with t'Data.Time.Clock.DiffTime' type from package @time@:
--
-- > import qualified Data.Time.Clock as T
-- >
-- > difftime :: T.DiffTime
-- >
-- > diffTimeToTimeOfDay difftime
--
-- Example with the 'Data.Time.LocalTime.TimeOfDay' type from package @time@:
--
-- > import qualified Data.Time.Clock as T
-- >
-- > timeofday :: T.TimeOfDay
-- >
-- > diffTimeToTimeOfDay $ T.timeOfDayToTime timeofday
diffTimeToTimeOfDay ::
    Real t
  => t
     -- ^ Number of non-leap seconds of the time of the day.
  -> TimeOfDay
diffTimeToTimeOfDay dt = do
  TimeOfDay
    { todHour = fromIntegral hours
    , todMin  = fromIntegral minutes
    , todSec  = fromIntegral seconds
    , todNSec = fromIntegral nsecs
    }
 where
  r :: Rational
  r = toRational dt
  (secs, nR) = properFraction r :: (Integer, Rational)
  nsecs :: Integer
  nsecs = round (nR * 1_000_000_000)
  (minsofday, seconds) = secs `divMod` 60 :: (Integer, Integer)
  (hours, minutes) = minsofday `divMod` 60 :: (Integer, Integer)
