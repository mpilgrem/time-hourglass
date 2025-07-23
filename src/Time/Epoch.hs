{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Time.Epoch
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Types and functions related to epochs.
-}

module Time.Epoch
  ( -- * Elapsed time from the start of epochs
    ElapsedSince (..)
  , ElapsedSinceP (..)
    -- * Epoch
  , Epoch (..)
    -- * Commonly-encountered epochs
  , UnixEpoch (..)
  , WindowsEpoch (..)
  ) where

import           Control.DeepSeq ( NFData (..) )
import           Data.Data ( Data )
import           Time.Time ( Time (..), Timeable (..) )
import           Time.Types
                   ( Elapsed (..), ElapsedP (..), NanoSeconds (..)
                   , Seconds (..)
                   )

-- | A type representing the number of seconds that have elapsed since the start
-- of a specified epoch.
newtype ElapsedSince epoch = ElapsedSince Seconds
  deriving (Data, Eq, NFData, Num, Ord,  Read, Show)

-- | A type representing the number of seconds and nanoseconds that have elapsed
-- since the start of a specified epoch. The \'P\' is short for \'precise\'.
data ElapsedSinceP epoch = ElapsedSinceP
  {-# UNPACK #-} !(ElapsedSince epoch)
  {-# UNPACK #-} !NanoSeconds
  deriving (Data, Eq, Ord, Read, Show)

instance NFData (ElapsedSinceP e) where
  rnf e = e `seq` ()

instance Num (ElapsedSinceP e) where
  (ElapsedSinceP e1 ns1) + (ElapsedSinceP e2 ns2) =
    ElapsedSinceP (e1 + e2) (ns1 + ns2)

  (ElapsedSinceP e1 ns1) - (ElapsedSinceP e2 ns2) =
    ElapsedSinceP (e1 - e2) (ns1 - ns2)

  (ElapsedSinceP e1 ns1) * (ElapsedSinceP e2 ns2) =
    ElapsedSinceP (e1 * e2) (ns1 * ns2)

  negate (ElapsedSinceP e ns) = ElapsedSinceP (negate e) ns

  abs (ElapsedSinceP e ns) = ElapsedSinceP (abs e) ns

  signum (ElapsedSinceP e ns) = ElapsedSinceP (signum e) ns

  fromInteger i = ElapsedSinceP (ElapsedSince (fromIntegral i)) 0

-- FIXME instance Real (ElapsedSinceP e)

-- | A type class promising epoch-related functionality.
--
class Epoch epoch where
  -- | The name of the epoch.
  epochName :: epoch -> String

  -- | The start of the epoch relative to the start of the Unix epoch
  -- (1970-01-01 00:00:00 UTC), in seconds. A negative number means the epoch
  -- starts before the starts of the Unix epoch.
  epochDiffToUnix :: epoch -> Seconds

-- | A type representing the Unix epoch, which started on
-- 1970-01-01 00:00:00 UTC.
data UnixEpoch = UnixEpoch
  deriving (Eq, Show)

instance Epoch UnixEpoch where
  epochName _ = "unix"
  epochDiffToUnix _ = 0

-- | A type representing the
-- [Windows epoch](https://learn.microsoft.com/en-us/windows/win32/sysinfo/file-times),
-- which started on 1601-01-01 00:00:00 UTC.
data WindowsEpoch = WindowsEpoch
  deriving (Eq, Show)

instance Epoch WindowsEpoch where
  epochName _ = "windows"
  epochDiffToUnix _ = -11644473600

instance Epoch epoch => Timeable (ElapsedSince epoch) where
  timeGetElapsedP es = ElapsedP (Elapsed e) 0
   where
    ElapsedSince e = convertEpoch es :: ElapsedSince UnixEpoch

  timeGetElapsed   es = Elapsed e
   where
    ElapsedSince e = convertEpoch es :: ElapsedSince UnixEpoch

  timeGetNanoSeconds _ = 0

instance Epoch epoch => Time (ElapsedSince epoch) where
  timeFromElapsedP (ElapsedP (Elapsed e) _) =
    convertEpoch (ElapsedSince e :: ElapsedSince UnixEpoch)

instance Epoch epoch => Timeable (ElapsedSinceP epoch) where
  timeGetElapsedP es = ElapsedP (Elapsed e) ns
   where
    ElapsedSinceP (ElapsedSince e) ns =
      convertEpochP es :: ElapsedSinceP UnixEpoch

  timeGetNanoSeconds (ElapsedSinceP _ ns) = ns

instance Epoch epoch => Time (ElapsedSinceP epoch) where
  timeFromElapsedP (ElapsedP (Elapsed e) ns) =
    convertEpochP (ElapsedSinceP (ElapsedSince e) ns :: ElapsedSinceP UnixEpoch)

-- | For the given pair of epochs, convert a t'ElapsedSince' value for the first
-- epoch to the corresponding value for the second epoch.
convertEpochWith ::
     (Epoch e1, Epoch e2)
  => (e1, e2)
  -> ElapsedSince e1
  -> ElapsedSince e2
convertEpochWith (e1, e2) (ElapsedSince s1) = ElapsedSince (s1 + diff)
 where
  diff = d1 - d2
  d1 = epochDiffToUnix e1
  d2 = epochDiffToUnix e2

-- | Convert the given t'ElapsedSince' value to another t'ElapsedSince' value.
-- This will not compile unless the compiler can infer the types of the epochs.
convertEpoch :: (Epoch e1, Epoch e2) => ElapsedSince e1 -> ElapsedSince e2
convertEpoch = convertEpochWith (undefined, undefined)

-- | For the given pair of epochs, convert a t'ElapsedSinceP' value for the
-- first epoch to the corresponding value for the second epoch.
convertEpochPWith ::
     (Epoch e1, Epoch e2)
  => (e1, e2)
  -> ElapsedSinceP e1
  -> ElapsedSinceP e2
convertEpochPWith es (ElapsedSinceP e1 n1) =
  ElapsedSinceP (convertEpochWith es e1) n1

-- | Convert the given t'ElapsedSinceP' value to another t'ElapsedSinceP' value.
-- This will not compile unless the compiler can infer the types of the epochs.
convertEpochP :: (Epoch e1, Epoch e2) => ElapsedSinceP e1 -> ElapsedSinceP e2
convertEpochP = convertEpochPWith (undefined, undefined)
