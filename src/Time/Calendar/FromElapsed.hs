{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Time.Calendar.FromElapsed
License     : BSD-style
Copyright   : (c) 2025 Mike Pilgrem <public@pilgrem.com>
Stability   : experimental
Portability : unknown

A native Haskell implementation of 'dateTimeFromUnixEpoch'.
-}

module Time.Calendar.FromElapsed
  ( dateTimeFromUnixEpoch
  ) where

import           Data.Int ( Int64 )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), Hours (..)
                   , Minutes (..), Seconds (..), TimeOfDay (..)
                   )

dateTimeFromUnixEpoch :: Elapsed -> DateTime
dateTimeFromUnixEpoch (Elapsed (Seconds secs)) =
  if secs >= 0
    then afterUnixEpoch secs
    else beforeUnixEpoch (negate secs)
 where

  isleap :: Int -> Bool
  isleap !y =
    let isDivBy n = y `mod` n == 0
    in   isDivBy 4 && not (isDivBy 100 || isDivBy 400)
  {-# INLINE isleap #-}

  days :: Int -> Int64
  days !y = if isleap y then 366 else 365
  {-# INLINE days #-}

  monthDays :: Bool -> Int -> Int64
  monthDays !isLeap !m = months !! m
   where
    months = if isLeap then leapMonths else nonLeapMonths
    leapMonths = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    nonLeapMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  {-# INLINE monthDays #-}

  yearLoop :: Int -> Int -> Int64 -> (Int, Int64)
  yearLoop !dir !y !d =
    let yd = days y
    in  if d <= yd
          then (y, d)
          else yearLoop dir (y + dir) (d - yd)

  monthLoop :: Bool -> Int -> Int -> Int64 -> (Int,  Int64, Int64)
  monthLoop !isLeap !dir !m !d =
    let md = monthDays isLeap m
    in  if d <= md
          then (m, d, md)
          else monthLoop isLeap dir (m + dir) (d - md)

  afterUnixEpoch :: Int64 -> DateTime
  afterUnixEpoch secs1 =
    let (!y, !m, !d, _, !h, !mi, !s) = fromUnixEpoch 1970 0 1 secs1
        date = Date y (toEnum m) (fromIntegral d + 1)
        tod = TimeOfDay (Hours h) (Minutes mi) (Seconds s) 0
    in  DateTime date tod

  beforeUnixEpoch :: Int64 -> DateTime
  beforeUnixEpoch secs1 =
    let (!y, !m, !d, !md, !h, !mi, !s) = fromUnixEpoch 1969 11 (-1) secs1
        date = Date y (toEnum m) (fromIntegral $ md - d)
        tod = TimeOfDay (Hours (23 - h)) (Minutes (59 - mi)) (Seconds (60 - s)) 0
    in  DateTime date tod

  fromUnixEpoch ::
       Int
    -> Int
    -> Int
    -> Int64
    -> (Int, Int, Int64, Int64, Int64, Int64, Int64)
  fromUnixEpoch !baseYear !baseMonth !dir !secs1 =
    let (!d1, !s1) = secs1 `quotRem` 86_400
        (!y, !d2) = yearLoop dir baseYear d1
        (!m, !d, !md) = monthLoop (isleap y) dir baseMonth d2
        (!h, !s2) = s1 `quotRem` 3_600
        (!mi, !s) = s2 `quotRem` 60
    in  (y, m, md, d, h, mi, s)
