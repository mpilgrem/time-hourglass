{- |
Module      : Time.Compat
License     : BSD-style
Copyright   : (c) 2015 Nicolas DI PRIMA <nicolas@di-prima.fr>

Basic time conversion compatibility.

This module aims to help conversion between types from @time@ package and types
from the @time-hourglass@ package.

An example of use (taken from file examples/Example/Time/Compat.hs):

> import Data.Hourglass as H
> import Data.Hourglass.Compat as C
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
>   newDate = C.dateFromTAIEpoch $ T.toModifiedJulianDay day
>
>   timeofday :: H.TimeOfDay
>   timeofday = C.diffTimeToTimeOfDay $ toRational $ T.timeOfDayToTime tod
>
>   offsetTime = H.TimezoneOffset $ fromIntegral tzmin
-}

module Time.Compat
  ( dateFromPOSIXEpoch
  , dateFromTAIEpoch
  , diffTimeToTimeOfDay
  ) where

import           Data.Hourglass.Time ( timeConvert )
import           Time.Types ( Date, Elapsed (..), TimeOfDay (..) )

-- | Given an integer which represents the number of days since the start of
-- the Unix epoch, yield the corresponding date in the proleptic Gregorian
-- calendar. Assumes that each day is 24 hours long.
dateFromPOSIXEpoch ::
     Integer
     -- ^ Number of days since the start of the Unix epoch.
  -> Date
dateFromPOSIXEpoch day = do
  let sec = Elapsed $ fromIntegral $ day * 86400
  timeConvert sec

-- | The number of days between 1858-11-17 00:00:00 UTC, the start of the
-- Modified Julian Date (MJD) epoch, and the Unix epoch
-- (1970-01-01 00:00:00 UTC).
--
-- The name of this function is a misnomer, as the International Atomic Time
-- (TAI) epoch starts on 1958-01-01 00:00:00 UTC.
daysTAItoPOSIX :: Integer
daysTAItoPOSIX = 40587

-- | Given an integer which represents the number of days since
-- 1858-11-17 00:00:00 UTC, the start of the Modified
-- Julian Date (MJD) epoch, yields the corresponding date in the proleptic
-- Gregorian calendar. Assumes that each day is 24 hours long.
--
-- The name of this function is a misnomer, as the International Atomic Time
-- (TAI) epoch starts on 1958-01-01 00:00:00 UTC.
--
-- This function allows a user to convert a t'Data.Time.Calendar.Day'
-- into t'Date'.
--
-- > import qualified Data.Time.Calendar as T
-- >
-- > timeDay :: T.Day
-- >
-- > dateFromTAIEpoch $ T.toModifiedJulianDay timeDay
dateFromTAIEpoch ::
     Integer
     -- ^ Number of days since 1858-11-17 00:00:00 UTC.
  -> Date
dateFromTAIEpoch dtai =
  dateFromPOSIXEpoch (dtai - daysTAItoPOSIX)

-- | Given a real number representing the number of seconds since the start of
-- the day, yield a t'TimeOfDay' value.
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
     -- ^ Number of seconds of the time of the day.
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
  nsecs = round (nR * 1000000000)
  (minsofday, seconds) = secs `divMod` 60 :: (Integer, Integer)
  (hours, minutes) = minsofday `divMod` 60 :: (Integer, Integer)
