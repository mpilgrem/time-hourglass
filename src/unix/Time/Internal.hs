{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Time.Internal
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

This module depends on the operating system. This is the version for Unix-like
operating systems.

Time lowlevel helpers for the unix operating system.

Depend on localtime_r and gmtime_r. Some obscure Unix system might not support
them.
-}

module Time.Internal
  ( dateTimeFromUnixEpochP
  , dateTimeFromUnixEpoch
  , systemGetTimezone
  , systemGetElapsed
  , systemGetElapsedP
  ) where

import           Foreign.C.Types ( CInt, CLong, CTime (..) )
import           Foreign.Marshal.Alloc ( alloca, allocaBytesAligned )
import           Foreign.Ptr ( Ptr, castPtr, nullPtr )
import           Foreign.Storable ( Storable (..) )
import           System.IO.Unsafe ( unsafePerformIO )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), ElapsedP (..)
                   , NanoSeconds (..), Seconds (..), TimeOfDay (..)
                   , TimezoneOffset (..)
                   )

-- | Convert a Unix epoch precise to t'DateTime'.
dateTimeFromUnixEpochP :: ElapsedP -> DateTime
dateTimeFromUnixEpochP (ElapsedP e ns) = fromCP ns $ rawGmTime e

-- | Convert a Unix epoch to t'DateTime'.
dateTimeFromUnixEpoch :: Elapsed -> DateTime
dateTimeFromUnixEpoch e = fromC $ rawGmTime e

-- | Return the timezone offset in minutes.
systemGetTimezone :: IO TimezoneOffset
systemGetTimezone = TimezoneOffset . fromIntegral . flip div 60 <$> localTime 0

--------------------------------------------------------------------------------
-- | Return the current elapsedP.
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

-- | Return the current elapsed.
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

foreign import ccall unsafe "gmtime_r"
  c_gmtime_r :: Ptr CTime -> Ptr CTm -> IO (Ptr CTm)

foreign import ccall unsafe "localtime_r"
  c_localtime_r :: Ptr CTime -> Ptr CTm -> IO (Ptr CTm)

-- | Return a global time's struct tm based on the number of elapsed second
-- since the start of the Unix epoch (1970-01-01 00:00:00 UTC).
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

-- | Represent the beginning of struct tm.
data CTm = CTm
  { ctmSec    :: CInt
  , ctmMin    :: CInt
  , ctmHour   :: CInt
  , ctmMDay   :: CInt
  , ctmMon    :: CInt
  , ctmYear   :: CInt
  }
  deriving (Eq, Show)

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

-- | Similar to 'fromC' except with nanosecond precision.
fromCP :: NanoSeconds -> CTm -> DateTime
fromCP ns ctm = DateTime d (t { todNSec = ns })
 where
  (DateTime d t) = fromC ctm

instance Storable CTm where
  alignment _ = 8
  sizeOf _    = 60 -- account for 9 ints, alignment + 2 unsigned long at end.
  peek ptr    = do
    CTm <$> peekByteOff intPtr 0
        <*> peekByteOff intPtr 4
        <*> peekByteOff intPtr 8
        <*> peekByteOff intPtr 12
        <*> peekByteOff intPtr 16
        <*> peekByteOff intPtr 20
   where
    intPtr = castPtr ptr
  poke ptr (CTm f0 f1 f2 f3 f4 f5) = do
    mapM_
      (uncurry (pokeByteOff intPtr))
      [(0, f0), (4, f1), (8, f2), (12, f3), (16, f4), (20, f5)]
    --pokeByteOff (castPtr ptr) 36 f9
   where
    intPtr = castPtr ptr
