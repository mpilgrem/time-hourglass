time-hourglass
==============

![Build Status](https://github.com/mpilgrem/time-hourglass/actions/workflows/tests.yml/badge.svg)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](http://haskell.org)

Originally forked from
[`hourglass-0.2.12`](https://hackage.haskell.org/package/hourglass-0.2.12).

Hourglass is a simple time library.

Documentation: [time-hourglass on hackage](http://hackage.haskell.org/package/time-hourglass)

Design
------
Key parts of the design are the Timeable and Time typeclasses.
Time representations of the same time values are interchangeable and easy to convert
between each other. This also allows the user to define new time types that
interact with the same functions as the built-in types.

For example:
```haskell
let dateTime0 =
      DateTime { dtDate = Date { dateYear = 1970, dateMonth = January, dateDay = 1 }
               , dtTime = TimeOfDay {todHour = 0, todMin = 0, todSec = 0, todNSec = 0 }}
    elapsed0 = Elasped 0

> timeGetElapsed elapsed0 == timeGetElapsed dateTime0
True
> timeGetDate elapsed0 == timeGetDate dateTime0
True
> timePrint "YYYY-MM" elapsed0
"1970-01"
> timePrint "YYYY-MM" dateTime0
"1970-01"
```

Hourglass has the same limitations as your system:

* On 32 bit linux, you can't get a date after the year 2038.
* In Windows 7, you can't get the date before the year 1601.

Comparaison with time
---------------------
* Getting posix time:
```haskell
-- With time
import Data.Time.Clock.POSIX

ptime <- getPOSIXTime

-- With hourglass
import System.Hourglass

ptime <- timeCurrent
```

* Getting the current year:
```haskell
-- With time
import Data.Time.Clock
import Data.Time.Calendar

currentYear <- (\(y,_,_) -> y) . toGregorian . utcDay <$> getCurrentTime

-- With hourglass
import System.Hourglass
import Data.Time

currentYear <- dateYear . timeGetDate <$> timeCurrent
```

* Representating "4th May 1970 15:12:24"
```haskell
-- With time
import Data.Time.Clock
import Date.Time.Calendar

let day = fromGregorian 1970 5 4
    diffTime = secondsToDiffTime (15 * 3600 + 12 * 60 + 24)
in UTCTime day diffTime

-- With hourglass
import Date.Time

DateTime (Date 1970 May 4) (TimeOfDay 15 12 24 0)
```

History
-------

The [`hourglass`](https://hackage.haskell.org/package/hourglass) package was
originated and then maintained by Vincent Hanquez. For published reasons, he
does not intend to develop the package further after version 0.2.12 but he also
does not want to introduce other maintainers.
