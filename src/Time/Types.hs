{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Time.Types
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>

Time-related types.

In principle, some units could hold infinite values. In practice, 'Int64' for
seconds (about @+/- 9e18@) and 'Int' for years is sufficient.
-}

module Time.Types
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
  , TimezoneOffset (..)
  , timezoneOffsetToSeconds
  , timezone_UTC
    -- * Conversion of periods of time
  , TimeInterval (..)
  ) where

import           Control.DeepSeq ( NFData (..) )
import           Data.Data ( Data )
import           Data.Int ( Int64 )
import           Data.Ratio ( (%) )
import           Time.Utils ( pad2 )

-- | Type class promising functionality for:
--
-- * converting a value of the type in question to a number of seconds; and
--
-- * converting a t'Seconds' value to a pair of a value of the type in question
--   and a remaining number of seconds.
class TimeInterval i where
  -- | For the given value, yield a corresponding number of seconds.
  toSeconds   :: i -> Seconds

  -- | For the given number of seconds, yield a pair of the corresponding value
  -- of the type in queston and a remaining number of seconds.
  fromSeconds :: Seconds -> (i, Seconds)

-- | Type representing numbers of nanoseconds.
newtype NanoSeconds = NanoSeconds Int64
  deriving (Data, Eq, NFData, Num, Ord, Read)

instance Show NanoSeconds where
  show (NanoSeconds v) = shows v "ns"

instance TimeInterval NanoSeconds where
  toSeconds (NanoSeconds ns) = Seconds (ns `div` 1000000000)
  fromSeconds (Seconds s) = (NanoSeconds (s * 1000000000), 0)

-- | Type representing numbers of seconds.
newtype Seconds = Seconds Int64
  deriving (Data, Eq, Enum, Integral, NFData, Num, Ord, Read, Real)

instance Show Seconds where
  show (Seconds s) = shows s "s"

instance TimeInterval Seconds where
  toSeconds   = id
  fromSeconds s = (s, 0)

-- | Type representing numbers of minutes.
newtype Minutes = Minutes Int64
  deriving (Data, Eq, Enum, Integral, NFData, Num, Ord, Read, Real)

instance Show Minutes where
  show (Minutes s) = shows s "m"

instance TimeInterval Minutes where
  toSeconds (Minutes m)   = Seconds (m * 60)
  fromSeconds (Seconds s) = (Minutes m, Seconds s')
   where
    (m, s') = s `divMod` 60

-- | Type representing numbers of hours.
newtype Hours = Hours Int64
  deriving (Data, Eq, Enum, Integral, NFData, Num, Ord, Read, Real)

instance Show Hours where
  show (Hours s) = shows s "h"

instance TimeInterval Hours where
  toSeconds (Hours h)     = Seconds (h * 3600)
  fromSeconds (Seconds s) = (Hours h, Seconds s')
   where
    (h, s') = s `divMod` 3600

-- | Type representing numbers of seconds elapsed since the start of the Unix
-- epoch (1970-01-01 00:00:00 UTC).
newtype Elapsed = Elapsed Seconds
  deriving (Data, Eq, NFData, Num, Ord, Read)

instance Show Elapsed where
  show (Elapsed s) = show s

-- | Type representing numbers of seconds and nanoseconds elapsed since the
-- start of the Unix epoch (1970-01-01 00:00:00 UTC).
data ElapsedP = ElapsedP {-# UNPACK #-} !Elapsed {-# UNPACK #-} !NanoSeconds
  deriving (Data, Eq, Ord, Read)

instance Show ElapsedP where
  show (ElapsedP e ns) = shows e ('.' : show ns)

instance NFData ElapsedP where
  rnf e = e `seq` ()

instance Num ElapsedP where
  (+) = addElapsedP

  (-) = subElapsedP

  (ElapsedP e1 ns1) * (ElapsedP e2 ns2) = ElapsedP (e1*e2) (ns1*ns2)

  negate (ElapsedP e ns) = ElapsedP (negate e) ns

  abs (ElapsedP e ns)    = ElapsedP (abs e) ns

  signum (ElapsedP e ns) = ElapsedP (signum e) ns

  fromInteger i          = ElapsedP (Elapsed (fromIntegral i)) 0

addElapsedP :: ElapsedP -> ElapsedP -> ElapsedP
addElapsedP (ElapsedP e1 (NanoSeconds ns1)) (ElapsedP e2 (NanoSeconds ns2)) =
  let notNormalizedNS = ns1 + ns2
      (retainedNS, ns) = notNormalizedNS `divMod` 1000000000
  in  ElapsedP (e1 + e2 + Elapsed (Seconds retainedNS)) (NanoSeconds ns)

subElapsedP :: ElapsedP -> ElapsedP -> ElapsedP
subElapsedP (ElapsedP e1 (NanoSeconds ns1)) (ElapsedP e2 (NanoSeconds ns2)) =
  let notNormalizedNS = ns1 - ns2
      notNormalizedS  = e1 - e2
  in  if notNormalizedNS < 0
        then
          ElapsedP
            (notNormalizedS - oneSecond)
            (NanoSeconds (1000000000 + notNormalizedNS))
        else
          ElapsedP notNormalizedS (NanoSeconds notNormalizedNS)
 where
  oneSecond :: Elapsed
  oneSecond = Elapsed $ Seconds 1

instance Real ElapsedP where
  toRational (ElapsedP (Elapsed (Seconds s)) (NanoSeconds 0)) =
    fromIntegral s

  toRational (ElapsedP (Elapsed (Seconds s)) (NanoSeconds ns)) =
    fromIntegral s + (fromIntegral ns % 1000000000)

-- | Type representing months of the Julian or Gregorian year.
data Month =
    January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Bounded, Data, Eq, Enum, Ord, Read, Show)

-- | Type representing days of the week. The enumeration starts on Sunday.
data WeekDay =
    Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Bounded, Data, Eq, Enum, Ord, Read, Show)

-- | Type representing offsets in minutes against UTC to obtain local time from
-- UTC. A positive number represents a location east of where UTC is local time
-- and a negative number represents a location west of where UTC is local time.
--
-- LocalTime t (-300) -- t represents a time at UTC-5
--
-- LocalTime t (+480) -- t represents a time at UTC+8
--
-- Should be between @(-12 * 60)@ and @(+14 * 60)@.
--
-- For example, in timezone AEDT (Australian Eastern Daylight Time) (UTC+11),
-- local time is 15:47. Consequently, UTC time is 04:47 and the timezone offset
-- is v'TimezoneOffset' @660@ (in minutes).
newtype TimezoneOffset = TimezoneOffset
  { timezoneOffsetToMinutes :: Int -- ^ The number of minutes.
  }
  deriving (Data, Eq, NFData, Ord)

-- | For the given timezone offset, yield the corresponding number of seconds.
timezoneOffsetToSeconds :: TimezoneOffset -> Seconds
timezoneOffsetToSeconds (TimezoneOffset ofs) = Seconds (fromIntegral ofs * 60)

instance Show TimezoneOffset where
  show (TimezoneOffset tz) =
    concat [if tz < 0 then "-" else "+", pad2 tzH, pad2 tzM]
   where
    (tzH, tzM) = abs tz `divMod` 60

-- | The UTC timezone.
--
-- > timezoneOffsetToMinutes timezone_UTC == 0 -- True
timezone_UTC :: TimezoneOffset
timezone_UTC = TimezoneOffset 0

-- | Type representing dates in the proleptic Gregorian calendar (the common
-- calendar).
data Date = Date
  { dateYear  :: {-# UNPACK #-} !Int
    -- ^ Year of the Common Era.
  , dateMonth :: !Month
    -- ^ Month of the year.
  , dateDay   :: {-# UNPACK #-} !Int
    -- ^ Day of the month, between 1 to 31.
  }
  deriving (Data, Eq, Ord, Read, Show)

instance NFData Date where
  rnf (Date y m d) = y `seq` m `seq` d `seq` ()

-- | Type representing times as hour, minutes, seconds and nanoseconds.
data TimeOfDay = TimeOfDay
  { todHour :: {-# UNPACK #-} !Hours
    -- ^ Hours, between 0 and 23.
  , todMin  :: {-# UNPACK #-} !Minutes
    -- ^ Minutes, between 0 and 59.
  , todSec  :: {-# UNPACK #-} !Seconds
    -- ^ Seconds, between 0 and 59. 60 when having leap second.
  , todNSec :: {-# UNPACK #-} !NanoSeconds
    -- ^ Nanoseconds, between 0 and 999999999.
  }
  deriving (Data, Eq, Ord, Read, Show)

instance NFData TimeOfDay where
  rnf (TimeOfDay h m s ns) = h `seq` m `seq` s `seq` ns `seq` ()

-- | Type representing date and time.
data DateTime = DateTime
  { dtDate :: Date
  , dtTime :: TimeOfDay
  }
  deriving (Data, Eq, Ord, Read, Show)

instance NFData DateTime where
  rnf (DateTime d t) = rnf d `seq` rnf t
