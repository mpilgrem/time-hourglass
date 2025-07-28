{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Time.Calendar.FromElapsed
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

A native Haskell implementation of 'dateTimeFromUnixEpoch'.
-}

module Time.Calendar.FromElapsed
  ( dateTimeFromUnixEpoch
  ) where

import           Data.Int ( Int64 )
-- import           Time.Internal ( dateTimeFromUnixEpoch )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), Hours (..)
                   , Minutes (..), Seconds (..), TimeOfDay (..)
                   )

dateTimeFromUnixEpoch :: Elapsed -> DateTime
dateTimeFromUnixEpoch (Elapsed (Seconds secs)) =
  if secs >=0
    then dateTimeFromUnixEpoch' secs
    else dateTimeFromUnixEpoch'' (negate secs)
 where

  dateTimeFromUnixEpoch' :: Int64 -> DateTime
  dateTimeFromUnixEpoch' secs1 =
    let (d1, s1) = secs1 `divMod` 86_400
        (y, d2) = loop1 1970 d1
        (m, d) = loop2 (isleap y) 0 d2
        (h, s2) = s1 `divMod` 3_600
        (mi, s) = s2 `divMod` 60
        date = Date y (toEnum m) (fromIntegral d + 1)
        tod = TimeOfDay (Hours h) (Minutes mi) (Seconds s) 0
    in  DateTime date tod
   where
    loop1 y d =
      let yd = days y
      in  if d <= yd
            then (y, d)
            else loop1 (y + 1) (d - yd)
    loop2 isLeap m d =
      let md = monthDays isLeap m
      in  if d <= md
            then (m, d)
            else loop2 isLeap (m + 1) (d - md)

  isleap y =
    let isDivBy n = y `mod` n == 0
    in   isDivBy 4 && not (isDivBy 100 && not (isDivBy 400))

  days y = if isleap y then 366 else 365

  monthDays _  0 = 31
  monthDays isLeap  1 = if isLeap then 29 else 28
  monthDays _  2 = 31
  monthDays _  3 = 30
  monthDays _  4 = 31
  monthDays _  5 = 30
  monthDays _  6 = 31
  monthDays _  7 = 31
  monthDays _  8 = 30
  monthDays _  9 = 31
  monthDays _ 10 = 30
  monthDays _ 11 = 31
  monthDays _ _  = undefined

  dateTimeFromUnixEpoch'' :: Int64 -> DateTime
  dateTimeFromUnixEpoch'' secs2 =
    let (d1, s1) = secs2 `divMod` 86_400
        (y, d2) = loop1 1969 d1
        (m, d) = loop2 (isleap y) 11 d2
        (h, s2) = s1 `divMod` 3_600
        (mi, s) = s2 `divMod` 60
        date = Date y (toEnum m) (fromIntegral d)
        tod = TimeOfDay (Hours (23 - h)) (Minutes (59 - mi)) (Seconds (60 - s)) 0
    in  DateTime date tod
   where
    loop1 y d =
      let yd = days y
      in  if d <= yd
            then (y, d)
            else loop1 (y - 1) (d - yd)
    loop2 isLeap m d =
      let md = monthDays isLeap m
      in  if d <= md
            then (m, md - d)
            else loop2 isLeap (m - 1) (d - md)
