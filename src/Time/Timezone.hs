{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      : Time.Timezone
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Timezone utilities.
-}

module Time.Timezone
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
  -- The default implementation is an ±HH:MM encoding of the 'timezoneOffset',
  -- with an offset of @0@ encoded as @-00:00@.
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

-- | Print a minute offset in format: ±HH:MM. An offset of @0@ is encoded as
-- @-00:00@.
tzMinutesPrint :: Int -> String
tzMinutesPrint offset =
  sign : (pad0 h ++ ":" ++ pad0 m)
 where
  (h, m) = abs offset `divMod` 60
  pad0 v
    | v < 10    = '0':show v
    | otherwise = show v
  sign = if offset > 0
    then
      '+'
    else
      -- This may be following the 'Unknown Local Offset Convention' in section
      -- 4.3 of RFC 3339 'Date and Time on the Internet: Timestamps', namely:
      --
      --     If the time in UTC is known, but the offset to local time is
      --     unknown, this can be represented with an offset of "-00:00". This
      --     differs semantically from an offset of "Z" or "+00:00", which imply
      --     that UTC is the preferred reference point for the specified time.
      --     RFC2822 describes a similar convention for email.
      '-'
