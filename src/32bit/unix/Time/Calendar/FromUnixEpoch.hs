{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Time.Calendar.FromUnixEpoch
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

This module depends on the machine architecture and operating system. This is
the version for 32-bit Unix-like operating systems.

Seconds since the Unix epoch to date and time helper for Unix-like operating
systems.

Depends on gmtime_r. Some obscure Unix systems might not support it.
-}

module Time.Calendar.FromUnixEpoch
  ( dateTimeFromUnixEpoch
  ) where

import           Foreign.C.Types ( CTime (..) )
import           Foreign.Marshal.Alloc ( alloca )
import           Foreign.Ptr ( Ptr, nullPtr )
import           Foreign.Storable ( Storable (..) )
import           System.IO.Unsafe ( unsafePerformIO )
import           Time.System.Types ( CTm (..) )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), Seconds (..)
                   , TimeOfDay (..)
                   )

-- | Given a number of non-leap seconds elapsed since the Unix epoch, yield the
-- corresponding t'DateTime' value.
dateTimeFromUnixEpoch :: Elapsed -> DateTime
dateTimeFromUnixEpoch e = fromC $ rawGmTime e

foreign import ccall unsafe "gmtime_r"
  c_gmtime_r :: Ptr CTime -> Ptr CTm -> IO (Ptr CTm)

-- | Given a number of non-leap seconds elapsed since the Unix epoch
-- (1970-01-01 00:00:00 UTC), yield the corresponding global time's struct tm.
rawGmTime :: Elapsed -> CTm
rawGmTime (Elapsed (Seconds s)) = unsafePerformIO callTime
 where
  callTime =
    alloca $ \ctmPtr -> do
    alloca $ \ctimePtr -> do
      poke ctimePtr ctime
      r <- c_gmtime_r ctimePtr ctmPtr
      if r == nullPtr
        then error "gmTime failed"
        else peek ctmPtr
  ctime = fromIntegral s
{-# NOINLINE rawGmTime #-}

-- | Convert a C structure to a DateTime structure.
fromC :: CTm -> DateTime
fromC ctm = DateTime date time
 where
  date = Date
    { dateYear  = fromIntegral $ ctmYear ctm + 1900
    , dateMonth = toEnum $ fromIntegral $ ctmMon ctm
    , dateDay   = fromIntegral $ ctmMDay ctm
    }
  time = TimeOfDay
    { todHour = fromIntegral $ ctmHour ctm
    , todMin  = fromIntegral $ ctmMin ctm
    , todSec  = fromIntegral $ ctmSec ctm
    , todNSec = 0
    }
