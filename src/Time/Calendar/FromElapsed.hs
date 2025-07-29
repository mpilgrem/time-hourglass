{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedTuples      #-}

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

import           GHC.Exts
                   ( Int#, Int64#, (*#), eqInt64#, geInt64#, int64ToInt#
                   , intToInt64#, leInt64#, negateInt64#, plusInt64#, quotInt64#
                   , subInt64#, timesInt64#
                   )
import           GHC.Int ( Int64 (..) )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), Hours (..)
                   , Minutes (..), Seconds (..), TimeOfDay (..)
                   )

type Bool# = Int#

dateTimeFromUnixEpoch :: Elapsed -> DateTime
dateTimeFromUnixEpoch (Elapsed (Seconds secs)) =
  case geInt64# secs# zeroInt64# of
    1# -> afterUnixEpoch secs#
    _  -> beforeUnixEpoch (negateInt64# secs#)
 where

  !(I64# secs#) = secs

  zeroInt64# = intToInt64# 0#
  oneInt64#  = intToInt64# 1#

  sInDay#  = intToInt64# 86_400#
  sInHour# = intToInt64#  3_600#
  sInMin#  = intToInt64#     60#

  divMod# :: Int64# -> Int64# -> (# Int64#, Int64# #)
  divMod# x# y# =
    let q# = quotInt64# x# y#
        r# = subInt64# x# (timesInt64# q# y#)
    in  (# q#, r# #)

  mod# :: Int64# -> Int64# -> Int64#
  mod# x# y# =
    let !(# _, r# #) = divMod# x# y#
    in  r#

  not# :: Int# -> Int#
  not# 0# = 1#
  not# _  = 0#

  isleap# :: Int64# -> Bool#
  isleap# y# =
    let isDivBy# n# = eqInt64# (mod# y# n#) zeroInt64#
    in  isDivBy# ( intToInt64# 4# ) *#
          not#
            (  isDivBy# ( intToInt64# 100# )
            *# not# ( isDivBy# ( intToInt64# 400# ))
            )

  days# :: Int64# -> Int64#
  days# y# = case isleap# y# of
    1# -> intToInt64# 366#
    _  -> intToInt64# 365#

  monthDays# :: Bool# -> Int64# -> Int64#
  monthDays# isLeap# m# =
    case int64ToInt# m# of
      0# -> intToInt64# 31#
      1# -> feb#
      2# -> intToInt64# 31#
      3# -> intToInt64# 30#
      4# -> intToInt64# 31#
      5# -> intToInt64# 30#
      6# -> intToInt64# 31#
      7# -> intToInt64# 31#
      8# -> intToInt64# 30#
      9# -> intToInt64# 31#
      10# -> intToInt64# 30#
      11# -> intToInt64# 31#
      _   -> zeroInt64#
   where
    feb# = case isLeap# of
      1# -> intToInt64# 29#
      _  -> intToInt64# 28#

  afterUnixEpoch :: Int64# -> DateTime
  afterUnixEpoch secs1# =
    let !(# d1#, s1# #) = divMod# secs1# sInDay#
        !(# y#, d2# #) = loop1# ( intToInt64# 1970# ) d1#
        !(# m#, d# #) = loop2# (isleap# y#) zeroInt64# d2#
        !(# h#, s2# #) = divMod# s1# sInHour#
        !(# mi#, s# #) = divMod# s2# sInMin#
        date = Date (fromIntegral $ I64# y#) (toEnum $ fromIntegral $ I64# m#) (fromIntegral (I64# (plusInt64# d# oneInt64#)))
        tod = TimeOfDay (Hours ( I64# h# )) (Minutes ( I64# mi# )) (Seconds (I64# s#)) 0
    in  DateTime date tod
   where
    loop1# :: Int64# -> Int64# -> (# Int64#, Int64# #)
    loop1# y# d# =
      let yd# = days# y#
      in  case leInt64# d# yd# of
            1# -> (# y#, d# #)
            _  -> loop1# ( plusInt64# y# oneInt64# ) ( subInt64# d# yd# )
    loop2# :: Bool# -> Int64# -> Int64# -> (# Int64#, Int64# #)
    loop2# isLeap# m# d# =
      let md# = monthDays# isLeap# m#
      in  case leInt64# d# md# of
            1# -> (# m#, d# #)
            _  -> loop2# isLeap# ( plusInt64# m# oneInt64# ) ( subInt64# d# md# )

  beforeUnixEpoch :: Int64# -> DateTime
  beforeUnixEpoch secs2# =
    let !(# d1#, s1# #) = divMod# secs2# sInDay#
        !(# y#, d2# #) = loop1# (intToInt64# 1969#) d1#
        !(# m#, d# #) = loop2# (isleap# y#) (intToInt64# 11#) d2#
        !(# h#, s2# #) = divMod# s1# sInHour#
        !(# mi#, s# #) = divMod# s2# sInMin#
        date = Date ( fromIntegral $ I64# y# ) ( toEnum $ fromIntegral $ I64# m# ) (fromIntegral $ I64# d#)
        tod = TimeOfDay
               (Hours $ I64# $ subInt64# (intToInt64# 23#) h#)
               (Minutes $ I64# $ subInt64# (intToInt64# 59#) mi#)
               (Seconds $ I64# $ subInt64# (intToInt64# 60#) s#) 0
    in  DateTime date tod
   where
    loop1# :: Int64# -> Int64# -> (# Int64#, Int64# #)
    loop1# y# d# =
       let yd# = days# y#
       in  case leInt64# d# yd# of
             1# -> (# y#, d# #)
             _  -> loop1# ( subInt64# y# oneInt64# ) ( subInt64# d# yd# )
    loop2# :: Bool# -> Int64# -> Int64# -> (# Int64#, Int64# #)
    loop2# isLeap# m# d# =
      let md# = monthDays# isLeap# m#
      in  case leInt64# d# md# of
            1# -> (# m#, subInt64# md# d# #)
            _  -> loop2# isLeap# ( subInt64# m# oneInt64# ) ( subInt64# d# md# )
