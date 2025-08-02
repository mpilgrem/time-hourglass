{-# LANGUAGE NumericUnderscores #-}

module Main
  ( main
  ) where

import qualified Time.Calendar as C
import qualified Time.Calendar.FromElapsed as FE
import           Test.Tasty.Bench ( bench, bgroup, defaultMain, nf )

main :: IO ()
main = defaultMain
  [ bgroup  "fromEpoch (Time.Calendar)" $
      [ bench "2024-03-01" $ nf C.dateTimeFromUnixEpoch 1_709_337_599
      , bench "2024-02-29" $ nf C.dateTimeFromUnixEpoch 1_709_251_199
      ]
  , bgroup  "fromEpoch (Time.Calendar.FromElapsed)" $
      [ bench "2024-03-01" $ nf FE.dateTimeFromUnixEpoch 1_709_337_599
      , bench "2024-02-29" $ nf FE.dateTimeFromUnixEpoch 1_709_251_199
      ]
  ]
