{- |
Module      : Data.Hourglass
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Time-related types and functions.

Basic types for representing points in time are t'Elapsed' and t'ElapsedP`. The
\'P\' is short for \'precise\'. t'Elapsed' represents numbers of seconds elapsed
since the start of the Unix epoch (1970-01-01 00:00:00 UTC). t'ElapsedP'
represents numbers of seconds and nanoseconds elapsed.

Values of other types representing points in time can be converted to and from
values of the t'Elapsed' and t'ElapsedP' types. For example:

> d = timeGetElapsed (Date 1955 April 18) :: Elapsed
> timeFromElapsed d :: DateTime

Local time is represented by t'LocalTime' @t@, parameterised by any other type
representing time (for example, t'Elapsed', t'Date' or t'DateTime'). A local
time value is augmented by a timezone offset in minutes. For example:

> localTime (Date 2014 May 4) 600 -- local time at UTC+10 of 4th May 2014
-}

module Data.Hourglass
  ( -- * Time units
    NanoSeconds (..)
  , Seconds (..)
  , Minutes (..)
  , Hours (..)
    -- * Calendar enumerations
  , Month (..)
  , WeekDay (..)
    -- * Points in time
    -- ** Elapsed time since the start of the Unix epoch
  , Elapsed (..)
  , ElapsedP (..)
    -- ** Date, time, and date and time
  , Date (..)
  , TimeOfDay (..)
  , DateTime (..)
    -- ** Local time and timezone-related
  , LocalTime
  , Timezone (..)
  , TimezoneOffset (..)
  , timezoneOffsetToSeconds
  , UTC (..)
  , timezone_UTC
  , TimezoneMinutes (..)
    -- *** Constructors
  , localTime
  , localTimeFromGlobal
  , localTimeSetTimezone
    -- *** Accessors
  , localTimeUnwrap
  , localTimeGetTimezone
    -- ** Miscellaneous calandar functions
  , isLeapYear
  , getWeekDay
  , getDayOfTheYear
  , daysInMonth
    -- * Periods of time
  , Duration (..)
  , Period (..)
  , timeAdd
  , timeDiff
  , timeDiffP
  , dateAddPeriod
    -- * Conversion of points in time
  , Time (..)
  , Timeable (..)
  , timeConvert
  , timeGetDate
  , timeGetDateTimeOfDay
  , timeGetTimeOfDay
  , localTimeConvert
  , localTimeToGlobal
    -- * Conversion of periods of time
  , TimeInterval (..)
    -- * Parsing and Printing
    -- ** Format strings
  , TimeFormatElem (..)
  , TimeFormatFct (..)
  , TimeFormatString (..)
  , TimeFormat (..)
    -- ** Common built-in formats
  , ISO8601_Date (..)
  , ISO8601_DateAndTime (..)
    -- ** Format methods
  , timePrint
  , timeParse
  , timeParseE
  , localTimePrint
  , localTimeParse
  , localTimeParseE
  ) where

import           Data.Hourglass.Calendar
                   ( daysInMonth, getDayOfTheYear, getWeekDay, isLeapYear )
import           Data.Hourglass.Format
import           Data.Hourglass.Local
import           Data.Hourglass.Time
import           Time.Types
import           Data.Hourglass.Zone
