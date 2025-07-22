time-hourglass
==============

![Build Status](https://github.com/mpilgrem/time-hourglass/actions/workflows/tests.yml/badge.svg)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](http://haskell.org)

Originally forked from
[`hourglass-0.2.12`](https://hackage.haskell.org/package/hourglass-0.2.12).

`time-hourglass` (originally `hourglass`) is a simple and efficient time
library.

Documentation
-------------

See the Haddock documentation on [Hackage](http://hackage.haskell.org/package/time-hourglass).

Design
------

A key part of the design is the `Timeable` and `Time` type classes. Types
representing time values that are instances of these classes allow easy
conversion between values of one time type and another.

For example:

~~~haskell
let dateTime0 =
      DateTime
        { dtDate = Date
            { dateYear = 1970
            , dateMonth = January
            , dateDay = 1
            }
        , dtTime = TimeOfDay
            { todHour = 0
            , todMin = 0
            , todSec = 0
            , todNSec = 0
            }
        }
    elapsed0 = Elasped 0

> timeGetElapsed elapsed0 == timeGetElapsed dateTime0
True
> timeGetDate elapsed0 == timeGetDate dateTime0
True
> timePrint "YYYY-MM" elapsed0
"1970-01"
> timePrint "YYYY-MM" dateTime0
"1970-01"
~~~

The library has the same limitations as your operating system, namely:

* on 32-bit Linux, you can't get a date after the year 2038; and
* on Windows, you can't get a date before the year 1601.

Comparaison with the `time` package
-----------------------------------

*   Getting the elapsed time since 1970-01-01 00:00 UTC (POSIX time) from the
    system clock:

    ~~~haskell
    -- With time:
    import Data.Time.Clock.POSIX ( getPOSIXTime )

    ptime <- getPOSIXTime

    -- With time-hourglass:
    import System.Hourglass ( timeCurrent )

    ptime <- timeCurrent
    ~~~

*   Getting the current year:

    ~~~haskell
    -- With time:
    import Data.Time.Clock ( UTCTime (..) )
    import Data.Time.Clock.POSIX ( getCurrentTime )
    import Data.Time.Calendar ( toGregorian )

    currentYear <- (\(y, _, _) -> y) . toGregorian . utcDay <$> getCurrentTime

    -- With time-hourglass:
    import System.Hourglass ( timeCurrent )
    import Data.Hourglass ( Date (..), timeGetDate )

    currentYear <- dateYear . timeGetDate <$> timeCurrent
    ~~~

*   Representating "4th May 1970 15:12:24"

    ~~~haskell
    -- With time:
    import Data.Time.Clock ( UTCTime (..), secondsToDiffTime )
    import Date.Time.Calendar ( fromGregorian )

    let day = fromGregorian 1970 5 4
        diffTime = secondsToDiffTime (15 * 3600 + 12 * 60 + 24)
    in  UTCTime day diffTime

    -- With time-hourglass:
    import Date.Time ( Date (..), DateTime (..), TimeOfDay (..) )

    DateTime (Date 1970 May 4) (TimeOfDay 15 12 24 0)
    ~~~

History
-------

The [`hourglass`](https://hackage.haskell.org/package/hourglass) package was
originated and then maintained by Vincent Hanquez. For published reasons, he
does not intend to develop the package further after version 0.2.12 but he also
does not want to introduce other maintainers.
