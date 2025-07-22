{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      : Data.Hourglass.Time
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Types representing time.
-}

module Data.Hourglass.Time
  ( -- * Generic time classes
    Time (..)
  , Timeable (..)
    -- * Elapsed time
  , Elapsed (..)
  , ElapsedP (..)
    -- * Generic conversion
  , timeConvert
    -- * Date and Time
  , timeGetDate
  , timeGetDateTimeOfDay
  , timeGetTimeOfDay
    -- * Arithmetic
  , Duration (..)
  , Period (..)
  , TimeInterval (..)
  , timeAdd
  , timeDiff
  , timeDiffP
  , dateAddPeriod
  ) where

import           Data.Hourglass.Calendar ( dateTimeToUnixEpoch )
import           Data.Hourglass.Diff
                   ( Duration (..), Period (..), dateAddPeriod
                   , elapsedTimeAddSecondsP
                   )
import           Data.Hourglass.Internal
                   ( dateTimeFromUnixEpoch, dateTimeFromUnixEpochP )
import           Foreign.C.Types ( CTime (..) )
import           Time.Types
                   ( Date, DateTime (..), Elapsed (..), ElapsedP (..)
                   , NanoSeconds (..), Seconds (..), TimeInterval (..)
                   , TimeOfDay (..)
                   )

-- | A type class promising functionality for types that represent time values:
--
-- * conversion to t'ElapsedP' and t'Elapsed'; and
--
-- * return nanoseconds (@0@ when the type is not more precise than seconds).
--
class Timeable t where
  -- | Convert a time representation to the number of elapsed seconds and
  -- nanoseconds since the start of the Unix epoch.
  timeGetElapsedP :: t -> ElapsedP

  -- | Convert a time representation to the number of elapsed seconds since the
  -- start of the Unix epoch.
  --
  -- Defaults to 'timeGetElapsedP'.
  timeGetElapsed :: t -> Elapsed
  timeGetElapsed t = e
   where
    ElapsedP e _ = timeGetElapsedP t

  -- | Optionally, return the number of nanoseconds.
  --
  -- If the underlaying type does not provide sub-second precision, @0@ should be
  -- returned.
  --
  -- Defaults to 'timeGetElapsedP'. For efficiency, if the underlaying type does
  -- not provide sub-second precision, it is a good idea to override this
  -- method.
  timeGetNanoSeconds :: t -> NanoSeconds
  timeGetNanoSeconds t = ns where ElapsedP _ ns = timeGetElapsedP t

-- | A type class for types that represent time values promising conversion
-- from t'ElapsedP' values and t'Elapsed' values.
class Timeable t => Time t where
  -- | Convert from a number of elapsed seconds and nanoseconds since the start
  -- of the Unix epoch.
  timeFromElapsedP :: ElapsedP -> t

  -- | Convert from a number of elapsed seconds since the start of the Unix
  -- epoch.
  --
  -- Defaults to 'timeFromElapsedP'.
  timeFromElapsed :: Elapsed -> t
  timeFromElapsed e = timeFromElapsedP (ElapsedP e 0)

instance Timeable CTime where
  timeGetElapsedP c         = ElapsedP (timeGetElapsed c) 0

  timeGetElapsed  (CTime c) = Elapsed (Seconds $ fromIntegral c)

  timeGetNanoSeconds _ = 0

instance Time CTime where
  timeFromElapsedP (ElapsedP e _)       = timeFromElapsed e

  timeFromElapsed (Elapsed (Seconds c)) = CTime (fromIntegral c)

instance Timeable Elapsed where
  timeGetElapsedP  e = ElapsedP e 0

  timeGetElapsed   e = e

  timeGetNanoSeconds _ = 0

instance Time Elapsed where
  timeFromElapsedP (ElapsedP e _) = e

  timeFromElapsed  e = e

instance Timeable ElapsedP where
  timeGetElapsedP    e               = e
  timeGetNanoSeconds (ElapsedP _ ns) = ns

instance Time ElapsedP where
  timeFromElapsedP   e               = e

instance Timeable Date where
  timeGetElapsedP d  = timeGetElapsedP (DateTime d (TimeOfDay 0 0 0 0))

instance Time Date where
  timeFromElapsedP (ElapsedP elapsed _) = d
   where
    (DateTime d _) = dateTimeFromUnixEpoch elapsed

instance Timeable DateTime where
  timeGetElapsedP d = ElapsedP (dateTimeToUnixEpoch d) (timeGetNanoSeconds d)
  timeGetElapsed = dateTimeToUnixEpoch
  timeGetNanoSeconds (DateTime _ (TimeOfDay _ _ _ ns)) = ns

instance Time DateTime where
  timeFromElapsedP = dateTimeFromUnixEpochP

-- | Convert from one time representation to another. This will not compile
-- unless the compiler can infer the types.
--
-- Specialized functions are available for built-in types:
--
-- * 'timeGetDate'
--
-- * 'timeGetDateTimeOfDay'
--
-- * 'timeGetElapsed'
--
-- * 'timeGetElapsedP'
timeConvert :: (Timeable t1, Time t2) => t1 -> t2
timeConvert t1 = timeFromElapsedP (timeGetElapsedP t1)
{-# INLINE[2] timeConvert #-}
{-# RULES "timeConvert/ID" timeConvert = id #-}
{-# RULES "timeConvert/ElapsedP" timeConvert = timeGetElapsedP #-}
{-# RULES "timeConvert/Elapsed" timeConvert = timeGetElapsed #-}

-- | Get the date (year-month-day) from a time representation (a
-- specialization of 'timeConvert').
timeGetDate :: Timeable t => t -> Date
timeGetDate t = d where (DateTime d _) = timeGetDateTimeOfDay t
{-# INLINE[2] timeGetDate #-}
{-# RULES "timeGetDate/ID" timeGetDate = id #-}
{-# RULES "timeGetDate/DateTime" timeGetDate = dtDate #-}

-- | Get the time (hours:minutes:seconds) from a time representation (a
-- specialization of 'timeConvert').
timeGetTimeOfDay :: Timeable t => t -> TimeOfDay
timeGetTimeOfDay t = tod where (DateTime _ tod) = timeGetDateTimeOfDay t
{-# INLINE[2] timeGetTimeOfDay #-}
{-# RULES "timeGetTimeOfDay/Date" timeGetTimeOfDay = const (TimeOfDay 0 0 0 0) #-}
{-# RULES "timeGetTimeOfDay/DateTime" timeGetTimeOfDay = dtTime #-}

-- | Get the date and time from a time representation (a specialization of
-- 'timeConvert').
timeGetDateTimeOfDay :: Timeable t => t -> DateTime
timeGetDateTimeOfDay t = dateTimeFromUnixEpochP $ timeGetElapsedP t
{-# INLINE[2] timeGetDateTimeOfDay #-}
{-# RULES "timeGetDateTimeOfDay/ID" timeGetDateTimeOfDay = id #-}
{-# RULES "timeGetDateTimeOfDay/Date" timeGetDateTimeOfDay = flip DateTime (TimeOfDay 0 0 0 0) #-}

-- | Add some time interval to a time representation and returns this new time
-- representation.
--
-- Example:
--
-- > t1 `timeAdd` mempty { durationHours = 12 }
timeAdd :: (Time t, TimeInterval ti) => t -> ti -> t
timeAdd t ti =
  timeFromElapsedP $ elapsedTimeAddSecondsP (timeGetElapsedP t) (toSeconds ti)

-- | Get the difference in seconds between two time representations.
--
-- Effectively:
--
-- > t2 `timeDiff` t1 = t2 - t1
timeDiff :: (Timeable t1, Timeable t2) => t1 -> t2 -> Seconds
timeDiff t1 t2 = sec where (Elapsed sec) = timeGetElapsed t1 - timeGetElapsed t2

-- | Get the difference in seconds and nanoseconds between two time
-- representations.
--
-- Effectively:
--
-- > @t2 `timeDiffP` t1 = t2 - t1
timeDiffP :: (Timeable t1, Timeable t2) => t1 -> t2 -> (Seconds, NanoSeconds)
timeDiffP t1 t2 = (sec, ns)
 where
  (ElapsedP (Elapsed sec) ns) = timeGetElapsedP t1 - timeGetElapsedP t2
