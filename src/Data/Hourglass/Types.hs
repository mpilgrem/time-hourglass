{- |
Module      : Data.Hourglass.Types
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>

Basic times units and types.

While pratically some units could hold infinite values, for practical and
efficient purpose they are limited to int64 types for seconds and int types for
years.

Most units use the Unix epoch referential, but by no means reduce portability.
The Unix referential works under the Windows platform or any other platforms.
-}

module Data.Hourglass.Types
  {-# DEPRECATED "Use Time.Types instead. Will be removed from future versions of this package." #-}
  ( module Time.Types
  ) where

import Time.Types
