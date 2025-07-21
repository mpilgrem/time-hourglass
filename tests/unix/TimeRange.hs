{-
This module depends on the operating system. This is the version for
Unix-like operating systems.
-}

module TimeRange
  ( loElapsed
  , hiElapsed
  , dateRange
  ) where

import           Data.Int ( Int64 )
import           Foreign.C.Types ( CTime )
import           Foreign.Storable ( sizeOf )

isCTime64 :: Bool
isCTime64 = sizeOf (undefined :: CTime) == 8

loElapsed :: Int64
loElapsed =
  if isCTime64
    then -62135596800 -- ~ year 0
    else -(2^(28 :: Int))

hiElapsed :: Int64
hiElapsed =
  if isCTime64
    then 2^(55 :: Int) -- in a future far far away
    else 2^(29 :: Int) -- before the 2038 bug.

dateRange :: (Int, Int)
dateRange =
  if isCTime64
    then (1800, 2202)
    else (1960, 2036)
