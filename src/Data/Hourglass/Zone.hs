{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      : Data.Hourglass.Zone
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Timezone utilities.
-}

module Data.Hourglass.Zone
  ( Timezone (..)
  , UTC (..)
  , TimezoneMinutes (..)
  ) where

-- | A type class promising timezone-related functionality.
class Timezone tz where
  -- | Offset in minutes from UTC. Valid values should be between @-12 * 60@ and
  -- @+14 * 60@.
  timezoneOffset :: tz -> Int

  -- | The name of the timezone.
  --
  -- Default implementation is an +-HH:MM encoding of the 'timezoneOffset'.
  timezoneName :: tz -> String
  timezoneName = tzMinutesPrint . timezoneOffset

-- | Simple timezone containing the number of minutes difference with UTC.
--
-- Valid values should be between @-12 * 60@ and @+14 * 60@.
newtype TimezoneMinutes = TimezoneMinutes Int
  deriving (Eq, Ord, Show)

-- | Type representing Universal Time Coordinated (UTC).
data UTC = UTC
  deriving (Eq, Ord, Show)

instance Timezone UTC where
  timezoneOffset _ = 0
  timezoneName _   = "UTC"

instance Timezone TimezoneMinutes where
  timezoneOffset (TimezoneMinutes minutes) = minutes

-- | Print a minute offset in format: (+-)HH:MM.
tzMinutesPrint :: Int -> String
tzMinutesPrint offset =
      (if offset > 0 then '+' else '-')
    : (pad0 h ++ ":" ++ pad0 m)
 where
  (h, m) = abs offset `divMod` 60
  pad0 v
    | v < 10    = '0':show v
    | otherwise = show v
