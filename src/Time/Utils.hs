{- |
Module      : Time.Utils
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Some padding / formatting functions.
-}

module Time.Utils
  ( pad2
  , pad3
  , pad4
  , padN
  ) where

-- | Pad a non-negative integer modulo 100 to 2 digits.
pad2 :: (Show a, Ord a, Num a, Integral a) => a -> String
pad2 v
  | v < 0     = undefined
  | v >= 100  = pad2 (v `mod` 100)
  | v >= 10   = show v
  | otherwise = '0' : show v

-- | Pad a non-negative integer less than 1,000 to 3 digits.
pad3 :: (Show a, Ord a, Num a, Integral a) => a -> String
pad3 v
  | v >= 1000 = undefined
  | v >= 100  = show v
  | v >= 10   = '0' : show v
  | otherwise = '0':'0': show v

-- | Pad a non-negative integer to at least 4 digits.
pad4 :: (Show a, Ord a, Num a, Integral a) => a -> String
pad4 v
  | v < 0 = undefined
  | v >= 1000 = show v
  | v >= 100  = '0' : show v
  | v >= 10   = '0':'0' : show v
  | otherwise = '0':'0':'0': show v

-- | Pad a non-negative integer to at least N digits.
--
-- If the number is greater, no truncation happens.
padN :: (Show a, Ord a, Num a, Integral a) => Int -> a -> String
padN n v
  | v < 0     = undefined
  | vlen >= n = vs
  | otherwise = replicate (n - vlen) '0' ++ vs
 where
  vs = show v
  vlen = length vs
