{-# LANGUAGE NumericUnderscores #-}

module Main
  ( main
  ) where

import           Time.Calendar ( dateTimeFromUnixEpoch )
import           Test.Tasty.Bench ( bench, bgroup, defaultMain, nf )

main :: IO ()
main = defaultMain
  [ bgroup  "fromEpoch (Time.Calendar)" $
      [ bench "2024-03-01" $ nf dateTimeFromUnixEpoch 1_709_337_599
      , bench "2024-02-29" $ nf dateTimeFromUnixEpoch 1_709_251_199
      ]
  ]
