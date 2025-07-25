{- |
Module      : Time.Utils
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Some padding / formatting functions.
-}

module Time.Utils where

-- | Pad a number to 2 digits.
pad2 :: (Show a, Ord a, Num a, Integral a) => a -> String
pad2 v
  | v >= 100   = pad2 (v `mod` 100)
  | v >= 10    = show v
  | otherwise  = '0' : show v

-- | Pad a number to 4 digits.
pad4 :: (Show a, Ord a, Num a, Integral a) => a -> String
pad4 v
  | v >= 1000  = show v
  | v >= 100   = '0' : show v
  | v >= 10    = '0':'0' : show v
  | otherwise  = '0':'0':'0': show v

-- | Pad a number to at least N digits.
--
-- If the number is greater, no truncation happens.
padN :: (Show a, Ord a, Num a, Integral a) => Int -> a -> String
padN n v
  | vlen >= n = vs
  | otherwise = replicate (n - vlen) '0' ++ vs
 where
  vs = show v
  vlen = length vs
