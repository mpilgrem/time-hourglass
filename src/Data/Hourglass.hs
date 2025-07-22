{- |
Module      : Data.Hourglass
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Types and functions for time manipulation.

The most basic type for time representation is t'Elapsed', which represent a
number seconds elapsed since the start of the Unix epoch.

Values of other typess can be converted to and from value of the t'Elapsed'
type:

> timeGetElapsed (Date 1 2 3) :: Elapsed
> timeFromElapsed 123         :: DateTime

Local time is represented by any other time type
(t'Elapsed', t'Date', t'DateTime', ..), but augmented by a timezone offset in
minutes.

> localTime (Date 2014 May 4) 600 -- local time at UTC+10 of May 4th 2014
-}

module Data.Hourglass
  ( module Data.Hourglass.Time
  , module Time.Types
  , module Data.Hourglass.Format
  , module Data.Hourglass.Local
  , module Data.Hourglass.Zone
    -- * Calendar miscellaneous functions
  , isLeapYear
  , getWeekDay
  , getDayOfTheYear
  , daysInMonth
  ) where

import           Data.Hourglass.Calendar
                   ( daysInMonth, getDayOfTheYear, getWeekDay, isLeapYear )
import           Data.Hourglass.Format
import           Data.Hourglass.Local
import           Data.Hourglass.Time
import           Time.Types
import           Data.Hourglass.Zone
