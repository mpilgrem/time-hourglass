{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Time.Types
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>

Basic times units and types.

While pratically some units could hold infinite values, for practical and
efficient purpose they are limited to int64 types for seconds and int types for
years.

Most units use the Unix epoch referential, but by no means reduce portability.
The Unix referential works under the Windows platform or any other platforms.
-}

module Time.Types
  ( -- * Time units
    NanoSeconds (..)
  , Seconds (..)
  , Minutes (..)
  , Hours (..)
  , TimeInterval (..)
    -- * Time enumeration
  , Month (..)
  , WeekDay (..)
    -- * Timezone
  , TimezoneOffset (..)
  , timezoneOffsetToSeconds
  , timezone_UTC
    -- * Computer friendly format
    -- ** Unix elapsed
  , Elapsed (..)
  , ElapsedP (..)
    -- * Human friendly format
    -- ** Calendar time
  , Date (..)
  , TimeOfDay (..)
  , DateTime (..)
  ) where

import           Control.DeepSeq
import           Data.Data
import           Data.Hourglass.Utils ( pad2 )
import           Data.Int
import           Data.Ratio

-- | Represent any time interval that has an equivalent value to a number of
-- seconds.
class TimeInterval i where
  toSeconds   :: i -> Seconds
  fromSeconds :: Seconds -> (i, Seconds)

-- | Nanoseconds.
newtype NanoSeconds = NanoSeconds Int64
  deriving (Data, Eq, NFData, Num, Ord, Read)

instance Show NanoSeconds where
  show (NanoSeconds v) = shows v "ns"

instance TimeInterval NanoSeconds where
  toSeconds (NanoSeconds ns) = Seconds (ns `div` 1000000000)
  fromSeconds (Seconds s) = (NanoSeconds (s * 1000000000), 0)

-- | Number of seconds without a referential.
--
-- Can hold a number between [-2^63,2^63-1], which should
-- be good for some billions of years.
--
-- However, because of limitation in the calendar conversion
-- currently used, seconds should be in the range [-2^55,2^55-1],
-- which is good for only 1 billion of year.
newtype Seconds = Seconds Int64
  deriving (Data, Eq, Enum, Integral, NFData, Num, Ord, Read, Real)

instance Show Seconds where
  show (Seconds s) = shows s "s"

instance TimeInterval Seconds where
  toSeconds   = id
  fromSeconds s = (s,0)

-- | Number of minutes without a referential.
newtype Minutes = Minutes Int64
  deriving (Data, Eq, Enum, Integral, NFData, Num, Ord, Read, Real)

instance Show Minutes where
  show (Minutes s) = shows s "m"

instance TimeInterval Minutes where
  toSeconds (Minutes m)   = Seconds (m * 60)
  fromSeconds (Seconds s) = (Minutes m, Seconds s')
   where
    (m, s') = s `divMod` 60

-- | Number of hours without a referential.
newtype Hours = Hours Int64
  deriving (Data, Eq, Enum, Integral, NFData, Num, Ord, Read, Real)

instance Show Hours where
  show (Hours s) = shows s "h"

instance TimeInterval Hours where
  toSeconds (Hours h)     = Seconds (h * 3600)
  fromSeconds (Seconds s) = (Hours h, Seconds s')
   where
    (h, s') = s `divMod` 3600

-- | A number of seconds elapsed since the unix epoch.
newtype Elapsed = Elapsed Seconds
  deriving (Data, Eq, NFData, Num, Ord, Read)

instance Show Elapsed where
  show (Elapsed s) = show s

-- | A number of seconds and nanoseconds elapsed since the unix epoch.
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

-- | Month of the year.
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

-- | Day of the week.
--
-- The enumeration starts on Sunday.
data WeekDay =
    Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Bounded, Data, Eq, Enum, Ord, Read, Show)

-- | Offset against UTC in minutes to obtain from UTC time, local time.
--
-- * a positive number represent a location East of UTC.
--
-- * a negative number represent a location West of UTC.
--
-- LocalTime t (-300) = t represent a time at UTC-5
-- LocalTime t (+480) = t represent a time at UTC+8
--
-- should be between -11H and +14H
--
-- Example:
--    in AUSEDT (UTC+1000 with daylight = UTC+1100), local time is 15:47;
--    Thus, UTC time is 04:47, and TimezoneOffset is +660 (minutes)
--
newtype TimezoneOffset = TimezoneOffset
  { timezoneOffsetToMinutes :: Int -- ^ return the number of minutes
  }
  deriving (Data, Eq, NFData, Ord)

-- | Return the number of seconds associated with a timezone.
timezoneOffsetToSeconds :: TimezoneOffset -> Seconds
timezoneOffsetToSeconds (TimezoneOffset ofs) = Seconds (fromIntegral ofs * 60)

instance Show TimezoneOffset where
  show (TimezoneOffset tz) =
    concat [if tz < 0 then "-" else "+", pad2 tzH, pad2 tzM]
   where
    (tzH, tzM) = abs tz `divMod` 60

-- | The UTC timezone. offset of 0.
timezone_UTC :: TimezoneOffset
timezone_UTC = TimezoneOffset 0

-- | Human date representation using common calendar.
data Date = Date
  { dateYear  :: {-# UNPACK #-} !Int   -- ^ year (Common Era)
  , dateMonth :: !Month                -- ^ month of the year
  , dateDay   :: {-# UNPACK #-} !Int   -- ^ day of the month, between 1 to 31
  }
  deriving (Data, Eq, Ord, Read, Show)

instance NFData Date where
  rnf (Date y m d) = y `seq` m `seq` d `seq` ()

-- | human time representation of hour, minutes, seconds in a day.
data TimeOfDay = TimeOfDay
  { todHour :: {-# UNPACK #-} !Hours
    -- ^ Hours, between 0 and 23
  , todMin  :: {-# UNPACK #-} !Minutes
    -- ^ Minutes, between 0 and 59
  , todSec  :: {-# UNPACK #-} !Seconds
    -- ^ Seconds, between 0 and 59. 60 when having leap second
  , todNSec :: {-# UNPACK #-} !NanoSeconds
    -- ^ Nanoseconds, between 0 and 999999999
  }
  deriving (Data, Eq, Ord, Read, Show)

instance NFData TimeOfDay where
  rnf (TimeOfDay h m s ns) = h `seq` m `seq` s `seq` ns `seq` ()

-- | Date and Time.
data DateTime = DateTime
  { dtDate :: Date
  , dtTime :: TimeOfDay
  }
  deriving (Data, Eq, Ord, Read, Show)

instance NFData DateTime where
  rnf (DateTime d t) = rnf d `seq` rnf t
