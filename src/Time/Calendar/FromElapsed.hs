{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedTuples      #-}

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

import           Data.Bits ( shiftR )
import           GHC.Exts ( dataToTag#, timesInt2# )
import           GHC.Int ( Int (..), Int64 (..) )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), Hours (..)
                   , Minutes (..), Seconds (..), TimeOfDay (..)
                   )

dateTimeFromUnixEpoch :: Elapsed -> DateTime
dateTimeFromUnixEpoch (Elapsed (Seconds secs)) =
  dateTimeFromGPEpoch (secs + 62_162_035_200)

-- | The \'Gregorian proleptic\' (GP) epoch (0000-03-01 00:00:00) follows a leap
-- day and starts a 400-year pattern of leap days.
dateTimeFromGPEpoch :: Int64 -> DateTime
dateTimeFromGPEpoch secs =
  let (!d1, !s1) = quotRem86400 secs
      (!q400, !r400) = quotRem146097 d1
      (!q100, !r100) = minDivMod 36_524 quotRem36524 r400
      (!q4, !r4) = quotRem1461 r100
      (!q1, !r1) =  minDivMod 365 quotRem365 r4
      !y' = fromIntegral $ 400 * q400 + 100 * q100 + 4 * q4 + q1
      !d2 = fromIntegral r1
      !m' = quot153 (5 * d2 + 2)
      !d = d2 - quot5 (153 * m' + 2) + 1
      (!h, !s2) = quotRem3600 s1
      (!mi, !s) = quotRem60 s2
      (!y, !m) = if m' > 9 then (y' + 1, m' - 10) else (y', m' + 2)
      !date = Date y (toEnum m) d
      !tod = TimeOfDay (Hours h) (Minutes mi) (Seconds s) 0
  in  DateTime { dtDate = date, dtTime = tod }
 where

  minDivMod :: Int64 -> (Int64 -> (Int64, Int64)) -> Int64 -> (Int64, Int64)
  minDivMod !d !quotRemD !n =
    let (q, r) = quotRemD n
    in  if q == 4 then (3, r + d) else (q, r)
  {-# INLINE minDivMod #-}

  quot5 :: Int -> Int
  quot5 = quotN1 1 7_378_697_629_483_820_647
  {-# INLINE quot5 #-}

  quot153 :: Int -> Int
  quot153 = quotN1 5 3_858_142_551_364_089_227
  {-# INLINE quot153 #-}

  quotN1 :: Int -> Int -> Int -> Int
  quotN1 !rShift !magic w_0 =
    let w1 w_1 = fromIntegral (I# (dataToTag# (w_1 < 0)))
    in  ((`shiftR` rShift) . (`mulHiInt` magic)) w_0 + w1 w_0
  {-# INLINE quotN1 #-}

  quotRem60 :: Int64 -> (Int64, Int64)
  quotRem60 = quotRemN2 60 5 (-8_608_480_567_731_124_087)
  {-# INLINE quotRem60 #-}

  quotRem365 :: Int64 -> (Int64, Int64)
  quotRem365 = quotRemN1 365 6 3_234_497_591_006_606_311
  {-# INLINE quotRem365 #-}

  quotRem1461 :: Int64 -> (Int64, Int64)
  quotRem1461 = quotRemN2 1_461 10 (-5_517_609_281_458_640_695)
  {-# INLINE quotRem1461 #-}

  quotRem3600 :: Int64 -> (Int64, Int64)
  quotRem3600 = quotRemN1 3_600 10 5_247_073_869_855_161_349
  {-# INLINE quotRem3600 #-}

  quotRem36524 :: Int64 -> (Int64, Int64)
  quotRem36524 = quotRemN2 36_524 15 (-1_896_998_432_287_073_591)
  {-# INLINE quotRem36524 #-}

  quotRem86400 :: Int64 -> (Int64, Int64)
  quotRem86400 = quotRemN1 86_400 13 1_749_024_623_285_053_783
  {-# INLINE quotRem86400 #-}

  quotRem146097 :: Int64 -> (Int64, Int64)
  quotRem146097 = quotRemN1 146_097 15 4_137_408_090_565_272_301
  {-# INLINE quotRem146097 #-}

  quotRemN1 :: Int64 -> Int -> Int64 -> Int64 -> (Int64, Int64)
  quotRemN1 !n !rShift !magic !w_0 =
    let q = w2 w_0
        w2 w_2 = ((`shiftR` rShift) . (`mulHiInt64` magic)) w_2 + w3 w_2
        w3 w_3 = fromIntegral (I# (dataToTag# (w_3 < 0)))
    in  (q, w_0 - (n * q))
  {-# INLINE quotRemN1 #-}

  quotRemN2 :: Int64 -> Int -> Int64 -> Int64 -> (Int64, Int64)
  quotRemN2 !n !rShift !magic !w_0 =
    let q = w2 w_0
        w2 w_2 = ((`shiftR` rShift) . w3) w_2 + w4 w_2
        w3 w_3 = w_3 + (`mulHiInt64` magic) w_3
        w4 w_4 = fromIntegral (I# (dataToTag# (w_4 < 0)))
    in  (q, w_0 - (n * q))
  {-# INLINE quotRemN2 #-}

  mulHiInt64 :: Int64 -> Int64 -> Int64
  mulHiInt64 x y = fromIntegral (fromIntegral x `mulHiInt` fromIntegral y :: Int)
  {-# INLINE mulHiInt64 #-}

  mulHiInt :: Int -> Int -> Int
  mulHiInt (I# x) (I# y) = let !(# _, hi, _ #) = timesInt2# x y in I# hi
  {-# INLINE mulHiInt #-}
