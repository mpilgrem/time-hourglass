{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      : Data.Hourglass.Time
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Types representing points in time, and associated type classes, and durations
of time, and associated functions.
-}

module Data.Hourglass.Time
  ( -- * Classes for conversion
    Time (..)
  , Timeable (..)
    -- * Conversion
  , timeConvert
    -- * Date and time
  , timeGetDate
  , timeGetDateTimeOfDay
  , timeGetTimeOfDay
    -- * Arithmetic
  , Duration (..)
  , Period (..)
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

-- | A type class promising functionality for:
--
-- * converting a value of the type in question to a t'Elapsed' value or
--   a t'ElapsedP' value; and
--
-- * yielding separately a nanoseconds component of the value of the type in
--   question (should yield @0@ when the type is less precise than seconds).
--
class Timeable t where
  -- | Convert the given value to the number of elapsed seconds and nanoseconds
  -- since the start of the Unix epoch (1970-01-01 00:00:00 UTC).
  timeGetElapsedP :: t -> ElapsedP

  -- | Convert the given value to the number of elapsed seconds since the start
  -- of the Unix epoch (1970-01-01 00:00:00 UTC).
  --
  -- Defaults to 'timeGetElapsedP'.
  timeGetElapsed :: t -> Elapsed
  timeGetElapsed t = e
   where
    ElapsedP e _ = timeGetElapsedP t

  -- | Optionally, for the given value, yield the number of nanoseconds
  -- component.
  --
  -- If the type in question does not provide sub-second precision, should yield
  -- @0@.
  --
  -- Defaults to 'timeGetElapsedP'. For efficiency, if the type in question does
  -- not provide sub-second precision, it is a good idea to override this
  -- method.
  timeGetNanoSeconds :: t -> NanoSeconds
  timeGetNanoSeconds t = ns where ElapsedP _ ns = timeGetElapsedP t

-- | A type class promising functionality for converting t'ElapsedP' values
-- and t'Elapsed' values to values of the type in question.
class Timeable t => Time t where
  -- | Convert from a number of elapsed seconds and nanoseconds since the start
  -- of the Unix epoch (1970-01-01 00:00:00 UTC).
  timeFromElapsedP :: ElapsedP -> t

  -- | Convert from a number of elapsed seconds since the start of the Unix
  -- epoch (1970-01-01 00:00:00 UTC).
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

-- | For the given value of a point in time, yield the corresponding date (a
-- specialization of 'timeConvert').
timeGetDate :: Timeable t => t -> Date
timeGetDate t = d where (DateTime d _) = timeGetDateTimeOfDay t
{-# INLINE[2] timeGetDate #-}
{-# RULES "timeGetDate/ID" timeGetDate = id #-}
{-# RULES "timeGetDate/DateTime" timeGetDate = dtDate #-}

-- | For the given value for a point in time, yield the corresponding time
-- (a specialization of 'timeConvert').
timeGetTimeOfDay :: Timeable t => t -> TimeOfDay
timeGetTimeOfDay t = tod where (DateTime _ tod) = timeGetDateTimeOfDay t
{-# INLINE[2] timeGetTimeOfDay #-}
{-# RULES "timeGetTimeOfDay/Date" timeGetTimeOfDay = const (TimeOfDay 0 0 0 0) #-}
{-# RULES "timeGetTimeOfDay/DateTime" timeGetTimeOfDay = dtTime #-}

-- | For the given value for a point in time, yield the corresponding date and
-- time (a specialization of 'timeConvert').
timeGetDateTimeOfDay :: Timeable t => t -> DateTime
timeGetDateTimeOfDay t = dateTimeFromUnixEpochP $ timeGetElapsedP t
{-# INLINE[2] timeGetDateTimeOfDay #-}
{-# RULES "timeGetDateTimeOfDay/ID" timeGetDateTimeOfDay = id #-}
{-# RULES "timeGetDateTimeOfDay/Date" timeGetDateTimeOfDay = flip DateTime (TimeOfDay 0 0 0 0) #-}

-- | Add the given period of time to the given value for a point time.
--
-- Example:
--
-- > t1 `timeAdd` mempty { durationHours = 12 }
timeAdd :: (Time t, TimeInterval ti) => t -> ti -> t
timeAdd t ti =
  timeFromElapsedP $ elapsedTimeAddSecondsP (timeGetElapsedP t) (toSeconds ti)

-- | For the two given points in time, yields the difference in seconds
-- between them.
--
-- Effectively:
--
-- > t2 `timeDiff` t1 = t2 - t1
timeDiff :: (Timeable t1, Timeable t2) => t1 -> t2 -> Seconds
timeDiff t1 t2 = sec where (Elapsed sec) = timeGetElapsed t1 - timeGetElapsed t2

-- | For the two given points in time, yields the difference in seconds and
-- nanoseconds between them.
--
-- Effectively:
--
-- > @t2 `timeDiffP` t1 = t2 - t1
timeDiffP :: (Timeable t1, Timeable t2) => t1 -> t2 -> (Seconds, NanoSeconds)
timeDiffP t1 t2 = (sec, ns)
 where
  (ElapsedP (Elapsed sec) ns) = timeGetElapsedP t1 - timeGetElapsedP t2
