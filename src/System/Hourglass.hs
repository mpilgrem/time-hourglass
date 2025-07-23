{- |
Module      : System.Hourglass
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Get the system timezone and current time value in multiple formats.
-}

module System.Hourglass
  {-# DEPRECATED "Use Time.System instead. Will be removed from future versions of this package." #-}
  ( module Time.System
  ) where

import Time.System
