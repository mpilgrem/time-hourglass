{- |
Module      : Time.Internal
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

This module depends on the operating system. This is the version for Windows.

Time lowlevel helpers binding to Windows.
-}

module Time.Internal
  ( dateTimeFromUnixEpochP
  , dateTimeFromUnixEpoch
  , systemGetTimezone
  , systemGetElapsed
  , systemGetElapsedP
  ) where

import           Data.Int ( Int64 )
import           System.IO.Unsafe ( unsafePerformIO )
import           System.Win32.Time
                   ( FILETIME (..), SYSTEMTIME (..), TimeZoneId (..)
                   , fileTimeToSystemTime, getSystemTimeAsFileTime
                   , getTimeZoneInformation, tziBias, tziDaylightBias
                   , tziStandardBias
                   )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), ElapsedP (..)
                   , NanoSeconds (..), Seconds (..), TimeOfDay (..)
                   , TimezoneOffset (..)
                   )

unixDiff :: Int64
unixDiff = 11644473600

toFileTime :: Elapsed -> FILETIME
toFileTime (Elapsed (Seconds s)) = FILETIME val
 where
  val = fromIntegral (s + unixDiff) * 10000000

toElapsedP :: FILETIME -> ElapsedP
toElapsedP (FILETIME w) = ElapsedP (Elapsed $ Seconds s) (NanoSeconds ns)
 where
  (sWin, hundredNs) = w `divMod` 10000000
  ns = fromIntegral (hundredNs * 100)
  s = fromIntegral sWin - unixDiff

toElapsed :: FILETIME -> Elapsed
toElapsed (FILETIME w) = Elapsed (Seconds s)
 where
  s = fromIntegral (fst (w `divMod` 10000000)) - unixDiff

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

systemGetTimezone :: IO TimezoneOffset
systemGetTimezone = do
  (tzMode,tzInfo) <- getTimeZoneInformation
  case tzMode of
    TzIdDaylight -> return $ toTO (tziBias tzInfo + tziDaylightBias tzInfo)
    TzIdStandard -> return $ toTO (tziBias tzInfo + tziStandardBias tzInfo)
    TzIdUnknown  -> return $ toTO (tziBias tzInfo)
 where
  -- a negative value represent value how to go from local to UTC,
  -- whereas TimezoneOffset represent the offset to go from UTC to local.
  -- here we negate the bias to get the proper value represented.
  toTO = TimezoneOffset . fromIntegral . negate

systemGetElapsedP :: IO ElapsedP
systemGetElapsedP = toElapsedP `fmap` getSystemTimeAsFileTime

systemGetElapsed :: IO Elapsed
systemGetElapsed = toElapsed `fmap` getSystemTimeAsFileTime
