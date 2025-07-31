{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Time.LocalTime
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

A local time is a global time together with a timezone.
-}

module Time.LocalTime
  ( -- * Local time
    -- ** Local time type
    LocalTime
    -- ** Local time creation and manipulation
  , localTime
  , localTimeUnwrap
  , localTimeToGlobal
  , localTimeFromGlobal
  , localTimeGetTimezone
  , localTimeSetTimezone
  , localTimeConvert
  ) where

import           Time.Diff ( elapsedTimeAddSecondsP )
import           Time.Time ( Time, Timeable (..), timeConvert )
import           Time.Types ( TimezoneOffset (..), timezoneOffsetToSeconds )

-- | Type representing local times.
data LocalTime t = LocalTime
  { localTimeUnwrap      :: t
    -- ^ The local time.
  , localTimeGetTimezone :: TimezoneOffset
    -- ^ The timezone offset.
  }

-- | Show the 'localTimeUnwrap' field and then the 'localTimeGetTimezone' field.
instance Show t => Show (LocalTime t) where
  show (LocalTime t tz) = show t ++ show tz

instance Eq t => Eq (LocalTime t) where
  LocalTime t1 tz1 == LocalTime t2 tz2 = tz1 == tz2 && t1 == t2

instance (Ord t, Time t) => Ord (LocalTime t) where
  compare l1@(LocalTime g1 tz1) l2@(LocalTime g2 tz2) =
    case compare tz1 tz2 of
      EQ -> compare g1 g2
      _  -> let t1 = localTimeToGlobal l1
                t2 = localTimeToGlobal l2
            in  compare t1 t2

instance Functor LocalTime where
  fmap f (LocalTime t tz) = LocalTime (f t) tz

-- | For the given timezone offset and time value (assumed to be the local
-- time), yield the corresponding local time.
localTime ::
     Time t
  => TimezoneOffset
  -> t
     -- ^ The local time.
  -> LocalTime t
localTime tz t = LocalTime t tz

-- | For the given t'LocalTime' value, yield the corresponding global time.
localTimeToGlobal :: Time t => LocalTime t -> t
localTimeToGlobal (LocalTime local tz)
  | tz == TimezoneOffset 0 = local
  | otherwise =
      timeConvert $ elapsedTimeAddSecondsP (timeGetElapsedP local) tzSecs
 where
  tzSecs = negate $ timezoneOffsetToSeconds tz

-- | For the given time value, yield the corresponding t'LocalTime' value
-- assuming that there is no timezone offset.
localTimeFromGlobal :: Time t => t -> LocalTime t
localTimeFromGlobal = localTime (TimezoneOffset 0)

-- | For the given timezone offset and local time, yield the corresponding local
-- time.
localTimeSetTimezone :: Time t => TimezoneOffset -> LocalTime t -> LocalTime t
localTimeSetTimezone tz currentLocal@(LocalTime t currentTz)
  | diffTz == 0 = currentLocal
  | otherwise   = LocalTime (timeConvert t') tz
 where
  t'        = elapsedTimeAddSecondsP (timeGetElapsedP t) diffTz
  diffTz    = timezoneOffsetToSeconds tz - timezoneOffsetToSeconds currentTz

-- | For the given local time of one type, yield the corresponding local time of
-- a different type. This will not compile unless the compiler can infer the
-- types of the two local times.
localTimeConvert :: (Time t1, Time t2) => LocalTime t1 -> LocalTime t2
localTimeConvert = fmap timeConvert
