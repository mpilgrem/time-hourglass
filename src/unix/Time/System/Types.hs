{- |
Module      : Time.System.Types
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

module Time.System.Types
  ( CTm (..)
  ) where

import           Foreign.C.Types ( CInt )
import           Foreign.Ptr ( castPtr )
import           Foreign.Storable ( Storable (..) )

-- | Represent the beginning of @struct tm@.
--
-- > struct tm
-- > {
-- >   int tm_sec;                   /* Seconds.     [0-60] (1 leap second) */
-- >   int tm_min;                   /* Minutes.     [0-59] */
-- >   int tm_hour;                  /* Hours.       [0-23] */
-- >   int tm_mday;                  /* Day.         [1-31] */
-- >   int tm_mon;                   /* Month.       [0-11] */
-- >   int tm_year;                  /* Year - 1900.  */
-- >   int tm_wday;                  /* Day of week. [0-6] */
-- >   int tm_yday;                  /* Days in year.[0-365] */
-- >   int tm_isdst;                 /* DST.         [-1/0/1]*/
-- >
-- >   int tm_mon_length;
-- >   int tm_year_length;
-- > };
data CTm = CTm
  { ctmSec  :: CInt
  , ctmMin  :: CInt
  , ctmHour :: CInt
  , ctmMDay :: CInt
  , ctmMon  :: CInt
  , ctmYear :: CInt
  }
  deriving (Eq, Show)

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
