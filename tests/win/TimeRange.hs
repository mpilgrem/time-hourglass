{-
This module depends on the operating system. This is the version for Windows.
-}

module TimeRange
  ( loElapsed
  , hiElapsed
  , dateRange
  ) where

import           Data.Int ( Int64 )

-- | Windows native functions to convert time cannot handle time before year
-- 1601.
loElapsed :: Int64
loElapsed = -11644473600 -- ~ year 1601

hiElapsed :: Int64
hiElapsed =  32503680000

dateRange :: (Int, Int)
dateRange = (1800, 2202)
