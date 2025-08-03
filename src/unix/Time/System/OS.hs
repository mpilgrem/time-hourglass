{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Time.System.OS
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

This module depends on the operating system. This is the version for Unix-like
operating systems.

Time low level helpers for Unix-like operating systems.

Depends on localtime_r. Some obscure Unix systems might not support it.
-}

module Time.System.OS
  ( systemGetTimezone
  , systemGetElapsed
  , systemGetElapsedP
  ) where

import           Foreign.C.Types ( CLong, CTime (..) )
import           Foreign.Marshal.Alloc ( alloca, allocaBytesAligned )
import           Foreign.Ptr ( Ptr, castPtr, nullPtr )
import           Foreign.Storable ( Storable (..) )
import           Time.System.Types ( CTm (..) )
import           Time.Types
                   ( Elapsed (..), ElapsedP (..), Seconds (..)
                   , TimezoneOffset (..)
                   )

-- | Return the timezone offset in minutes.
systemGetTimezone :: IO TimezoneOffset
systemGetTimezone = TimezoneOffset . fromIntegral . flip div 60 <$> localTime 0

--------------------------------------------------------------------------------
-- | Return the current number of non-leap seconds and nanoseconds elapsed since
-- the Unix epoch.
systemGetElapsedP :: IO ElapsedP
systemGetElapsedP = allocaBytesAligned sofTimespec 8 $ \ptr -> do
  c_clock_get ptr
  toElapsedP <$> peek (castPtr ptr) <*> peekByteOff ptr sofCTime
 where
  sofTimespec = sofCTime + sofCLong
  sofCTime = sizeOf (0 :: CTime)
  sofCLong = sizeOf (0 :: CLong)
  toElapsedP :: CTime -> CLong -> ElapsedP
  toElapsedP (CTime sec) nsec =
    ElapsedP (Elapsed $ Seconds (fromIntegral sec)) (fromIntegral nsec)

-- | Return the current number of non-leap seconds elapsed since the Unix epoch.
systemGetElapsed :: IO Elapsed
systemGetElapsed = allocaBytesAligned sofTimespec 8 $ \ptr -> do
  c_clock_get ptr
  toElapsed <$> peek (castPtr ptr)
 where
  sofTimespec = sizeOf (0 :: CTime) + sizeOf (0 :: CLong)
  toElapsed :: CTime -> Elapsed
  toElapsed (CTime sec) = Elapsed $ Seconds (fromIntegral sec)

foreign import ccall unsafe "hourglass_clock_calendar"
  c_clock_get :: Ptr CLong -> IO ()

foreign import ccall unsafe "localtime_r"
  c_localtime_r :: Ptr CTime -> Ptr CTm -> IO (Ptr CTm)

-- | Return a local time's gmtoff (seconds east of UTC).
--
-- Use the ill-defined gmtoff (at offset 40) that might or might not be
-- available for your platform. Worst case scenario it's not initialized
-- properly.
localTime :: Elapsed -> IO CLong
localTime (Elapsed (Seconds s)) = callTime
 where
  callTime =
    alloca $ \ctmPtr -> do
    alloca $ \ctimePtr -> do
      poke ctimePtr ctime
      r <- c_localtime_r ctimePtr ctmPtr
      if r == nullPtr
        then error "localTime failed"
        else peekByteOff ctmPtr 40
  ctime = fromIntegral s
