{- |
Module      : Data.Hourglass.Compat
License     : BSD-style
Copyright   : (c) 2015 Nicolas DI PRIMA <nicolas@di-prima.fr>

Basic Time conversion compatibility.

This module aims to help conversion between the types from the package time to
the package hourglass.

Example of use (extracted from file Example/Time/Compat.hs):

> import Data.Hourglass as H
> import Data.Hourglass.Compat as C
> import Data.Time as T
>
> transpose :: T.ZonedTime -> H.LocalTime H.DateTime
> transpose oldTime = H.localTime
>   offsetTime
>   (H.DateTime newDate timeofday)
>  where
>   T.ZonedTime (T.LocalTime day tod) (T.TimeZone tzmin _ _) = oldTime
>
>   newDate :: H.Date
>   newDate = C.dateFromTAIEpoch $ T.toModifiedJulianDay day
>
>   timeofday :: H.TimeOfDay
>   timeofday = C.diffTimeToTimeOfDay $ toRational $ T.timeOfDayToTime tod
>
>   offsetTime = H.TimezoneOffset $ fromIntegral tzmin

This module will be depreciated in favor of "Time.Compat".
-}

module Data.Hourglass.Compat
  ( module Time.Compat
  ) where

import Time.Compat
