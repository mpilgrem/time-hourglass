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
  dateTimeFromGPEpoch (secs + 719_468 * 86_400)

dateTimeFromGPEpoch :: Int64 -> DateTime
dateTimeFromGPEpoch secs =
  let (!d1, !s1) = secs `quotRem` 86_400
      (!q400, !r400) = d1 `quotRem` 146_097
      (!q100, !r100) = r400 `minDivMod` 36_524
      (!q4, !r4) = r100 `quotRem` 1461
      (!q1, !r1) = r4 `minDivMod` 365
      !y' = fromIntegral $ 400 * q400 + 100 * q100 + 4 * q4 + q1
      !d2 = fromIntegral r1
      !isLeap = q1 == 0 && (q4 /= 0 || q100 == 0)
      (!m', !d) = monthDays isLeap d2
      (!h, !s2) = s1 `quotRem` 3_600
      (!mi, !s) = s2 `quotRem` 60
      (y, m) = if m' > 9 then (y' + 1, m' - 10) else (y', m' + 2)
      !date = Date y (toEnum m) d
      !tod = TimeOfDay (Hours h) (Minutes mi) (Seconds s) 0
  in  DateTime { dtDate = date, dtTime = tod }
 where

  minDivMod :: Int64 -> Int64 -> (Int64, Int64)
  minDivMod !n !d =
    let (q, r) = n `quotRem` d
    in  if q == 4 then (3, r + d) else (q, r)
  {-# INLINE minDivMod #-}

  monthDays :: Bool -> Int -> (Int, Int)
  monthDays !isLeap !doy = go 0 table
   where
    table = if isLeap then leapTable else nonLeapTable
    go !i (a : b : rest)
      | doy < b   = (i, doy - a + 1)
      | otherwise = go (i + 1) (b : rest)
    go _ _ = (11, doy - table !! 11 + 1)  -- fallback for Dec
    nonLeapTable = [0, 31, 61, 92, 122, 153, 184, 214, 245, 275, 306, 337]
    leapTable   = [0, 31, 62, 93, 123, 154, 185, 215, 246, 276, 307, 338]
  {-# INLINE monthDays #-}
