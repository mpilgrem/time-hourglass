{- |
Module      : Time.System
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Get the system timezone and current time value in multiple formats.
-}

module Time.System
  (
    -- * Current time in computer friendly format
    timeCurrent
  , timeCurrentP
    -- * Current time in human friendly DateTime format
  , dateCurrent
  , localDateCurrent
  , localDateCurrentAt
    -- * System timezone
  , timezoneCurrent
  ) where

import           Data.Hourglass.Internal
                   ( systemGetElapsed, systemGetElapsedP, systemGetTimezone )
import           Data.Hourglass.Local
                   ( LocalTime, localTimeFromGlobal, localTimeSetTimezone )
import           Data.Hourglass.Time ( timeGetDateTimeOfDay )
import           Time.Types ( DateTime, Elapsed, ElapsedP, TimezoneOffset )

-- | Get the current elapsed seconds since epoch.
timeCurrent :: IO Elapsed
timeCurrent = systemGetElapsed

-- | Get the current elapsed seconds (precise to the nanosecond) since epoch.
timeCurrentP :: IO ElapsedP
timeCurrentP = systemGetElapsedP

-- | Get the current global date.
--
-- This is equivalent to:
--
-- > timeGetDateTimeOfDay `fmap` timeCurrentP
dateCurrent :: IO DateTime
dateCurrent = timeGetDateTimeOfDay <$> timeCurrentP

-- | Get the localized date by using 'timezoneCurrent' and 'dateCurrent'.
localDateCurrent :: IO (LocalTime DateTime)
localDateCurrent = localTimeSetTimezone
  <$> timezoneCurrent
  <*> (localTimeFromGlobal <$> dateCurrent)

-- | Get the localized date at a specific timezone offset.
localDateCurrentAt :: TimezoneOffset -> IO (LocalTime DateTime)
localDateCurrentAt tz =
  localTimeSetTimezone tz . localTimeFromGlobal <$> dateCurrent

-- | Get the current timezone offset.
--
-- This include daylight saving time when in operation.
timezoneCurrent :: IO TimezoneOffset
timezoneCurrent = systemGetTimezone
