{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Time.Calendar.FromUnixEpoch
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

This module depends on the operating system. This is the version for Windows.

Seconds since the Unix epoch to date and time helper for Windows.
-}

module Time.Calendar.FromUnixEpoch
  ( dateTimeFromUnixEpochP
  , dateTimeFromUnixEpoch
  ) where

import           Data.Int ( Int64 )
import           System.IO.Unsafe ( unsafePerformIO )
import           System.Win32.Time
                   ( FILETIME (..), SYSTEMTIME (..), fileTimeToSystemTime )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), ElapsedP (..)
                   , Seconds (..), TimeOfDay (..)
                   )

unixDiff :: Int64
unixDiff = 11_644_473_600

toFileTime :: Elapsed -> FILETIME
toFileTime (Elapsed (Seconds s)) = FILETIME val
 where
  val = fromIntegral (s + unixDiff) * 10_000_000

callSystemTime :: Elapsed -> SYSTEMTIME
callSystemTime e = unsafePerformIO (fileTimeToSystemTime (toFileTime e))
{-# NOINLINE callSystemTime #-}

dateTimeFromUnixEpochP :: ElapsedP -> DateTime
dateTimeFromUnixEpochP (ElapsedP e ns) = toDateTime $ callSystemTime e
 where
  toDateTime (SYSTEMTIME wY wM _ wD wH wMin wS _) =
    DateTime
      (Date (fi wY) (toEnum $ fi $ wM - 1) (fi wD))
      (TimeOfDay (fi wH) (fi wMin) (fi wS) ns)
  fi :: (Integral a, Num b) => a -> b
  fi = fromIntegral

dateTimeFromUnixEpoch :: Elapsed -> DateTime
dateTimeFromUnixEpoch e = toDateTime $ callSystemTime e
 where
  toDateTime (SYSTEMTIME wY wM _ wD wH wMin wS _) =
    DateTime
      (Date (fi wY) (toEnum $ fi $ wM - 1) (fi wD))
      (TimeOfDay (fi wH) (fi wMin) (fi wS) 0)
  fi :: (Integral a, Num b) => a -> b
  fi = fromIntegral
