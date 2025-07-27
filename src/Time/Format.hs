{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Time.Format
License     : BSD-style
Copyright   : (c) 2014 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Time formatting : printing and parsing

Built-in format strings
-}

module Time.Format
  ( -- * Parsing and Printing
    -- ** Format strings
    TimeFormat (..)
  , TimeFormatString (..)
  , TimeFormatElem (..)
  , TimeFormatFct (..)
    -- ** Common built-in formats
  , ISO8601_Date (..)
  , ISO8601_DateAndTime (..)
    -- ** Format methods
  , timePrint
  , timeParse
  , timeParseE
  , localTimePrint
  , localTimeParse
  , localTimeParseE
  ) where

import           Data.Char ( isDigit, isSpace, ord )
import           Data.Int ( Int64 )
import           Time.Calendar ( getDayOfTheYear )
import           Time.Internal ( dateTimeFromUnixEpochP )
import           Time.LocalTime
                   ( LocalTime (..), localTime, localTimeToGlobal )
import           Time.Time
                   ( Period (..), Timeable (..), dateAddPeriod
                   , timeGetDateTimeOfDay
                   )
import           Time.Timezone ( Timezone (..), TimezoneMinutes (..) )
import           Time.Utils ( pad2, pad4, padN )
import           Time.Types
                   ( Date (..), DateTime (..), Elapsed (..), ElapsedP (..)
                   , Hours (..), Minutes (..), Month (..), NanoSeconds (..)
                   , Seconds (..), TimeOfDay (..), TimezoneOffset (..)
                   , timezone_UTC
                   )

-- | Type representing formatters that can be part of a time format string.
data TimeFormatElem =
    Format_Year2
    -- ^ 2-digit years (@70@ is 1970, @69@ is 2069).
  | Format_Year4
    -- ^ 4-digit years.
  | Format_Year
    -- ^ Any digits years.
  | Format_Month
    -- ^ Months (@1@ to @12@).
  | Format_Month2
    -- ^ Months padded to 2 characters (@01@ to @12@).
  | Format_MonthName_Short
    -- ^ Short name of the month (@Jan@, @Feb@, ..).
  | Format_DayYear
    -- ^ Day of the year (@1@ to @365@, @366@ for leap years).
  | Format_Day
    -- ^ Day of the month (@1@ to @31@).
  | Format_Day2
    -- ^ Day of the month padded to 2 characters (@01@ to @31@).
  | Format_Hour
    -- ^ Hours padded to 2 characters (@00@ to @23@).
  | Format_Minute
    -- ^ Minutes padded to 2 characters (@00@ to @59@).
  | Format_Second
    -- ^ Seconds padded to 2 characters (@00@ to @59@, @60@ for leap seconds).
  | Format_UnixSecond
    -- ^ Number of non-leap seconds since the Unix epoch
    -- (1970-01-01 00:00:00 UTC).
  | Format_MilliSecond
    -- ^ The millisecond component only, padded to 3 characters (@000@ to
    -- @999@). See 'Format_MicroSecond' and 'Format_NanoSecond' for other
    -- named sub-second components.
  | Format_MicroSecond
    -- ^ The microseconds component only, padded to 3 characters (@000@ to
    -- @999@). See 'Format_MilliSecond' and 'Format_NanoSecond' for other
    -- named sub-second components.
  | Format_NanoSecond
    -- ^ The nanoseconds component only, padded to 3 characters (@000@ to
    -- @999@). See 'Format_MilliSecond' and 'Format_MicroSecond' for other
    -- named sub-second components.
  | Format_Precision Int
    -- ^ Sub-second display with a precision of n digits, with n between @1@
    -- and @9@.
  | Format_TimezoneName
    -- ^ Timezone name.
  | Format_TzHM_Colon_Z
    -- ^ Zero UTC offset (@Z@) or timezone offset with colon (for example,
    -- @02:00@, @+02:00@ or @-02:00@).
  | Format_TzHM_Colon
    -- ^ Timezone offset with colon (for example, @02:00@, @+02:00@ or
    -- @-02:00@).
  | Format_TzHM
    -- ^ Timezone offset without colon (for example, @0200@, @+0200@ or
    -- @-0200@).
  | Format_Tz_Offset
    -- ^ Timezone offset in minutes (for example, @120@, @+120@ or @-120@).
  | Format_Spaces
    -- ^ One or more space-like characters.
  | Format_Text Char
    -- ^ A verbatim character.
  | Format_Fct TimeFormatFct
    -- ^ A custom time format function. See v'TimeFormatFct'.
  deriving (Eq, Show)

-- | Type representing format functions.
data TimeFormatFct = TimeFormatFct
  { timeFormatFctName :: String
    -- ^ The name of the format function.
  , timeFormatParse ::
         (DateTime, TimezoneOffset)
      -> String
      -> Either String ((DateTime, TimezoneOffset), String)
    -- ^ A parser of a given 'String'. The first argument is the
    -- @(@v'DateTime'@,@ v'TimezoneOffset'@)@ value before the parser is
    -- applied. If the parser fails, the 'Left' value provides an error message.
    -- If it succeeds, the 'Right' value provides a pair of a
    -- @(@v'DateTime'@,@ v'TimezoneOffset'@)@ value and any input not consumed
    -- by the parser.
  , timeFormatPrint :: DateTime -> TimezoneOffset -> String
    -- ^ A printer of a given v'DateTime' value and v'TimezoneOffset' value.
  }

instance Show TimeFormatFct where
  show = timeFormatFctName

instance Eq TimeFormatFct where
  t1 == t2 = timeFormatFctName t1 == timeFormatFctName t2

-- | Type representing time format strings, composed of list
-- of t'TimeFormatElem'.
newtype TimeFormatString = TimeFormatString [TimeFormatElem]
  deriving (Eq, Show)

-- | A type class promising the ability to convert values to
-- a t'TimeFormatString'.
--
-- 'String' is an instance of 'TimeFormat'. Sequences of characters are
-- interpreted as follows (case-sensitive). The longest valid sequence is parsed
-- first:
--
-- [@YY@]:    'Format_Year2'. 2-digit years (@70@ is 1970, @69@ is 2069).
-- [@YYYY@]:  'Format_Year4'. 4-digit years.
-- [@M@]:     'Format_Month'. Months (@1@ to @12@).
-- [@MM@]:    'Format_Month2'. Months padded to 2 characters (@01@ to @12@).
-- [@Mon@]:   'Format_MonthName_Short'. Short name of the month (@Jan@, @Feb@,
--            ..).
-- [@DD@]:    'Format_Day2'. Day of the month padded to 2 characters (@01@ to
--            @31@).
-- [@H@]:     'Format_Hour'. Hours padded to 2 characters (@00@ to @23@).
-- [@MI@]:    'Format_Minute'. Minutes padded to 2 characters (@00@ to @59@).
-- [@S@]:     'Format_Second'. Seconds padded to 2 characters (@00@ to @59@,
--            @60@ for leap seconds).
-- [@EPOCH@]: 'Format_UnixSecond'. Number of non-leap seconds since the Unix
--            epoch (1970-01-01 00:00:00 UTC).
-- [@ms@]:    'Format_MilliSecond'. The millisecond component only, padded to 3
--            characters (@000@ to @999@). See @us@/@μ@ and @ns@ for other named
--            sub-second components.
-- [@us@]:    'Format_MicroSecond'. The microseconds component only, padded to 3
--            characters (@000@ to @999@). See @ms@ and @ns@ for other named
--            sub-second components.
-- [@μ@]:     'Format_MicroSecond'. As above.
-- [@ns@]:    'Format_NanoSecond'. The nanoseconds component only, padded to 3
--            characters (@000@ to @999@). See @ms@ and @us@/@μ@ for other named
--            sub-second components.
-- [@p\<n\>@]: 'Format_Precision' @\<n\>@. Sub-second display with a precision
--             of @\<n\>@ digit(s), where @\<n\>@ is @1@ to @9@.
-- [@TZH:M@]: 'Format_TzHM_Colon'. Timezone offset with colon (for example,
--            @02:00@, @+02:00@ or @-02:00@).
-- [@TZHM@]:  'Format_TzHM'. Timezone offset without colon (for example,
--            @0200@, @+0200@ or @-0200@).
-- [@TZOFS@]: 'Format_Tz_Offset'. Timezone offset in minutes (for example,
--            @120@, @+120@ or @-120@).
-- [@\<space\>@]:       'Format_Spaces'. One or more space-like characters.
-- [@\\\<character\>@]: 'Format_Text' @\<character\>@. A verbatim character.
-- [@\<character\>@]:   'Format_Text' @\<character\>@. A verbatim character.
--
-- For example:
--
-- >>> let mDateTime = timeParse ("ms \\ms us \\us ns \\ns") "123 ms 456 us 789 ns"
-- >>> timeGetNanoSeconds <$> mDateTime
-- Just 123456789ns
--
-- >>> timePrint "ms \\ms us \\us ns \\ns" <$> mDateTime
-- Just "123 ms 456 us 789 ns"
class TimeFormat format where
  toFormat :: format -> TimeFormatString

-- | A type representing a ISO8601 date format string.
--
-- e.g. 2014-04-05
data ISO8601_Date = ISO8601_Date
  deriving (Eq, Show)

-- | A type representing a ISO8601 date and time format string.
--
-- e.g. 2014-04-05T17:25:04+00:00 or 2014-04-05T17:25:04Z.
data ISO8601_DateAndTime = ISO8601_DateAndTime
  deriving (Eq, Show)

instance TimeFormat [TimeFormatElem] where
  toFormat = TimeFormatString

instance TimeFormat TimeFormatString where
  toFormat = id

-- | For information about this instance, see the documentation for
-- 'TimeFormat'.
instance TimeFormat String where
  toFormat = TimeFormatString . toFormatElem
   where
    toFormatElem []                  = []
    toFormatElem ('Y':'Y':'Y':'Y':r) = Format_Year4  : toFormatElem r
    toFormatElem ('Y':'Y':r)         = Format_Year2  : toFormatElem r
    toFormatElem ('M':'M':r)         = Format_Month2 : toFormatElem r
    toFormatElem ('M':'o':'n':r)     = Format_MonthName_Short : toFormatElem r
    toFormatElem ('M':'I':r)         = Format_Minute : toFormatElem r
    toFormatElem ('M':r)             = Format_Month  : toFormatElem r
    toFormatElem ('D':'D':r)         = Format_Day2   : toFormatElem r
    toFormatElem ('H':r)             = Format_Hour   : toFormatElem r
    toFormatElem ('S':r)             = Format_Second : toFormatElem r
    toFormatElem ('m':'s':r)         = Format_MilliSecond : toFormatElem r
    toFormatElem ('u':'s':r)         = Format_MicroSecond : toFormatElem r
    toFormatElem ('μ':r)             = Format_MicroSecond : toFormatElem r
    toFormatElem ('n':'s':r)         = Format_NanoSecond : toFormatElem r
    toFormatElem ('p':'1':r)         = Format_Precision 1 : toFormatElem r
    toFormatElem ('p':'2':r)         = Format_Precision 2 : toFormatElem r
    toFormatElem ('p':'3':r)         = Format_Precision 3 : toFormatElem r
    toFormatElem ('p':'4':r)         = Format_Precision 4 : toFormatElem r
    toFormatElem ('p':'5':r)         = Format_Precision 5 : toFormatElem r
    toFormatElem ('p':'6':r)         = Format_Precision 6 : toFormatElem r
    toFormatElem ('p':'7':r)         = Format_Precision 7 : toFormatElem r
    toFormatElem ('p':'8':r)         = Format_Precision 8 : toFormatElem r
    toFormatElem ('p':'9':r)         = Format_Precision 9 : toFormatElem r
    -----------------------------------------------------------
    toFormatElem ('E':'P':'O':'C':'H':r) = Format_UnixSecond : toFormatElem r
    -----------------------------------------------------------
    toFormatElem ('T':'Z':'H':'M':r)     = Format_TzHM : toFormatElem r
    toFormatElem ('T':'Z':'H':':':'M':r) = Format_TzHM_Colon : toFormatElem r
    toFormatElem ('T':'Z':'O':'F':'S':r) = Format_Tz_Offset : toFormatElem r
    -----------------------------------------------------------
    toFormatElem ('\\':c:r)          = Format_Text c : toFormatElem r
    toFormatElem (' ':r)             = Format_Spaces : toFormatElem r
    toFormatElem (c:r)               = Format_Text c : toFormatElem r

instance TimeFormat ISO8601_Date where
  toFormat _ =
    TimeFormatString [Format_Year, dash, Format_Month2, dash, Format_Day2]
   where
    dash = Format_Text '-'

instance TimeFormat ISO8601_DateAndTime where
  toFormat _ = TimeFormatString
    [ Format_Year, dash, Format_Month2, dash, Format_Day2 -- date
    , Format_Text 'T'
    , Format_Hour, colon, Format_Minute, colon, Format_Second -- time
    , Format_TzHM_Colon_Z
      -- Either timezone offset with colon (±HH:MM) or UTC zero offset (Z).
    ]
   where
    dash = Format_Text '-'
    colon = Format_Text ':'

monthFromShort :: String -> Either String Month
monthFromShort str =
  case str of
    "Jan" -> Right January
    "Feb" -> Right February
    "Mar" -> Right March
    "Apr" -> Right April
    "May" -> Right May
    "Jun" -> Right June
    "Jul" -> Right July
    "Aug" -> Right August
    "Sep" -> Right September
    "Oct" -> Right October
    "Nov" -> Right November
    "Dec" -> Right December
    _     -> Left $ "unknown month: " ++ str

printWith ::
     (TimeFormat format, Timeable t)
  => format
  -> t
  -> TimezoneOffset
  -> String
printWith fmt t tzOfs@(TimezoneOffset tz) = concatMap fmtToString fmtElems
 where
  fmtToString Format_Year     = show (dateYear date)
  fmtToString Format_Year4    = pad4 (dateYear date)
  fmtToString Format_Year2    = pad2 (dateYear date - 1900)
  fmtToString Format_Month2   = pad2 (fromEnum (dateMonth date) + 1)
  fmtToString Format_Month    = show (fromEnum (dateMonth date) + 1)
  fmtToString Format_MonthName_Short = take 3 $ show (dateMonth date)
  fmtToString Format_DayYear  = show (getDayOfTheYear date)
  fmtToString Format_Day2     = pad2 (dateDay date)
  fmtToString Format_Day      = show (dateDay date)
  fmtToString Format_Hour     = pad2 (fromIntegral (todHour tm) :: Int)
  fmtToString Format_Minute   = pad2 (fromIntegral (todMin tm) :: Int)
  fmtToString Format_Second   = pad2 (fromIntegral (todSec tm) :: Int)
  fmtToString Format_MilliSecond = padN 3 (ns `div` 1000000)
  fmtToString Format_MicroSecond = padN 3 ((ns `div` 1000) `mod` 1000)
  fmtToString Format_NanoSecond = padN 3 (ns `mod` 1000)
  fmtToString (Format_Precision n)
      | n >= 1 && n <= 9 = padN n (ns `div` (10 ^ (9 - n)))
      | otherwise        = error "invalid precision format"
  fmtToString Format_UnixSecond = show unixSecs
  fmtToString Format_TimezoneName = timezoneName $ TimezoneMinutes tz
  fmtToString Format_Tz_Offset = show tz
  fmtToString Format_TzHM = show tzOfs
  fmtToString Format_TzHM_Colon_Z
      | tz == 0   = "Z"
      | otherwise = fmtToString Format_TzHM_Colon
  fmtToString Format_TzHM_Colon =
      let (tzH, tzM) = abs tz `divMod` 60
          sign = if tz < 0 then "-" else "+"
       in sign ++ pad2 tzH ++ ":" ++ pad2 tzM
  fmtToString Format_Spaces   = " "
  fmtToString (Format_Text c) = [c]
  fmtToString (Format_Fct tff) = timeFormatPrint tff dateTime tzOfs

  (TimeFormatString fmtElems) = toFormat fmt

  (Elapsed (Seconds unixSecs)) = timeGetElapsed t
  dateTime@(DateTime date tm) = timeGetDateTimeOfDay t
  (NanoSeconds ns) = timeGetNanoSeconds t

-- | Given the specified format, pretty print the given local time.
--
-- A v'Format_TimezoneName' will print using the ±HH:MM format, where @0@
-- offset is printed as @-00:00@.
--
-- A v'Format_TzHM' will print using the ±HHMM format, where @0@ offset is
-- printed as @+0000@.
--
-- A v'Format_TzHM_Colon' will print using the ±HH:MM format, where @0@ offset
-- is printed as @+00:00@.
--
-- A v'Format_TzHM_Colon_Z' will print using the ±HH:MM format, but where @0@
-- offset is printed as @Z@.
--
-- A v'Format_Tz_Offset' will print non-negative offsets without using an
-- initial @+@.
--
-- A v'Format_Spaces' will print a single space character.
localTimePrint ::
     (TimeFormat format, Timeable t)
  => format      -- ^ The format to use for printing.
  -> LocalTime t -- ^ The local time to print.
  -> String
localTimePrint fmt lt =
  printWith fmt (localTimeUnwrap lt) (localTimeGetTimezone lt)

-- | Like 'localTimePrint' but the time zone of the time to print will be taken
-- to be UTC.
timePrint ::
     (TimeFormat format, Timeable t)
  => format -- ^ The format to use for printing.
  -> t      -- ^ The time to print.
  -> String
timePrint fmt t = printWith fmt t timezone_UTC

-- | Given the specified format, try to parse the given string as
-- a t'LocalTime' t'DateTime' value.
--
-- On failure, yields a 'Left' value with a pair of the
-- current t'TimeFormatElem' value and the reason for the failure.
--
-- If successful, yields a 'Right' value with a pair of the parsed value and
-- the remaining unparsed string.
--
-- The default parsed t'LocalTime' t'DateTime' value is \'all zeros'\. For
-- example:
--
-- >>> let zeroDate = Date 0 January 0
-- >>> let zeroTime = TimeOfDay 0 0 0 0
-- >>> let zeroLocalTime = localTime timezone_UTC (DateTime zeroDate zeroTime)
-- >>> localTimeParseE "" "" == Right (zeroLocalTime, "")
-- True
--
-- Later t'TimeFormatElem' values can modify the result of earlier
-- t'TimeFormatElem' values. For example:
--
-- >>> let toYear = dateYear . dtDate . localTimeUnwrap . fst
-- >>> toYear <$> (localTimeParseE "YYYY YYYY" "2025 2024")
-- Right 2024
--
-- A v'Format_DayYear' interprets the day of year based on the previously parsed
-- year or, by default, a leap year. For example:
--
-- >>> let toMonth = dateMonth . dtDate . localTimeUnwrap . fst
-- >>> let format1 = [Format_Year4, Format_Spaces, Format_DayYear]
-- >>> let format2 = [Format_DayYear, Format_Spaces, Format_Year4]
-- >>> toMonth <$> (localTimeParseE format1 "2025 60")
-- Right March
-- >>> toMonth <$> (localTimeParseE format2 "60 2025")
-- Right February
--
-- A v'Format_TimezoneName' will parse one or more non-white space characters
-- but will not modify the previously parsed, or default, date and time.
--
-- A v'Format_Month', v'Format_DayYear', v'Format_Day' and v'Format_Tz_Offset'
-- will check that the parsed number is within bounds. However,
-- 'localTimeParseE' does not check that any resulting date or time is a valid
-- one.
localTimeParseE ::
     TimeFormat format
  => format -- ^ The format to use for parsing.
  -> String -- ^ The string to parse.
  -> Either (TimeFormatElem, String) (LocalTime DateTime, String)
localTimeParseE fmt = loop ini fmtElems
 where
  (TimeFormatString fmtElems) = toFormat fmt

  toLocal (dt, tz) = localTime tz dt

  loop acc []    s  = Right (toLocal acc, s)
  loop _   (x:_) [] = Left (x, "empty")
  loop acc (x:xs) s =
    case processOne acc x s of
      Left err         -> Left (x, err)
      Right (nacc, s') -> loop nacc xs s'

  processOne ::
       (DateTime, TimezoneOffset)
    -> TimeFormatElem
    -> [Char]
    -> Either String ((DateTime, TimezoneOffset), [Char])
  processOne _   _               []     = Left "empty"
  processOne acc (Format_Text c) (x:xs)
    | c == x    = Right (acc, xs)
    | otherwise = Left ("unexpected char, got: " ++ show x)

  processOne acc Format_Year s =
    onSuccess (\y -> modDate (setYear y) acc) $ isNumber s
  processOne acc Format_Year4 s =
    onSuccess (\y -> modDate (setYear y) acc) $ getNDigitNum 4 s
  processOne acc Format_Year2 s = onSuccess
    ( \y -> let year = if y < 70 then y + 2000 else y + 1900
            in  modDate (setYear year) acc
    )
    $ getNDigitNum 2 s
  processOne acc Format_Month s =
    let result = isNumber s :: Either String (Int, String)
    in  case result of
          Left err -> Left err
          Right (m, s')
            | m > 0 && m <= 12 ->
                Right (modDate (setMonth (toEnum (m - 1))) acc, s')
            | otherwise -> Left ("month invalid, got: " <> show m)
  processOne acc Format_Month2 s = onSuccess
    ( \m -> modDate (setMonth $ toEnum ((fromIntegral m - 1) `mod` 12)) acc
    )
    $ getNDigitNum 2 s
  processOne acc Format_MonthName_Short s =
    onSuccess (\m -> modDate (setMonth m) acc) $ getMonth s
  processOne acc Format_DayYear s =
    let y = (dateYear . dtDate . fst) acc
        result = isNumber s :: Either String (Int, String)
    in  case result of
          Left err -> Left err
          Right (d, s')
              -- We can't be more helpful because we may not yet know the
              -- intended year
            | d > 0 && d <= 366 ->
                let p = Period 0 0 (d - 1)
                    startOfYear = Date y January 1
                in  Right (modDate (const (dateAddPeriod startOfYear p)) acc, s')
            | otherwise -> Left ("day of year invalid, got: " <> show d)
  processOne acc Format_Day s =
    let result = isNumber s :: Either String (Int, String)
    in  case result of
          Left err -> Left err
          Right (d, s')
              -- We can't be more helpful because we may not yet know the
              -- intended month and year
            | d > 0 && d <= 31 -> Right (modDate (setDay d) acc, s')
            | otherwise -> Left ("day of month invalid, got: " <> show d)
  processOne acc Format_Day2 s =
    onSuccess (\d -> modDate (setDay d) acc) $ getNDigitNum 2 s
  processOne acc Format_Hour s =
    onSuccess (\h -> modTime (setHour h) acc) $ getNDigitNum 2 s
  processOne acc Format_Minute s =
    onSuccess (\mi -> modTime (setMin mi) acc) $ getNDigitNum 2 s
  processOne acc Format_Second s =
    onSuccess (\sec -> modTime (setSec sec) acc) $ getNDigitNum 2 s
  processOne acc Format_MilliSecond s =
    onSuccess (\ms -> modTime (setNsMask (6,3) ms) acc) $ getNDigitNum 3 s
  processOne acc Format_MicroSecond s =
    onSuccess (\us -> modTime (setNsMask (3,3) us) acc) $ getNDigitNum 3 s
  processOne acc Format_NanoSecond s =
    onSuccess (\ns -> modTime (setNsMask (0,3) ns) acc) $ getNDigitNum 3 s
  processOne acc (Format_Precision p) s =
    onSuccess (\num -> modTime (setNS num) acc) $ getNDigitNum p s
  processOne acc Format_TimezoneName s = case break isSpace s of
    ("", _) -> Left ("no non-white space at start of: " <> s)
    (_, s2) -> Right (acc, s2)
  processOne acc Format_UnixSecond s =
    onSuccess (\sec ->
      let newDate =
            dateTimeFromUnixEpochP $ flip ElapsedP 0 $ Elapsed $ Seconds sec
      in  modDT (const newDate) acc) $ isNumber s
  processOne acc Format_TzHM_Colon_Z a@(c:s)
    | c == 'Z'  = Right (acc, s)
    | otherwise = processOne acc Format_TzHM_Colon a
  processOne acc Format_TzHM_Colon (c:s) =
    parseHMSign True acc c s
  processOne acc Format_TzHM (c:s) =
    parseHMSign False acc c s
  processOne acc Format_Tz_Offset s@(c:cs) = case c of
    '-' -> process True (12 * 60) cs
    '+' -> process False (14 * 60) cs
    _ -> process False (14 * 60) s
   where
    (dt, _) = acc
    process isNeg limit s' =
      let result = isNumber s' :: Either String (Int, String)
      in  case result of
            Left err -> Left err
            Right (mins, rest)
              | mins >= 0 && mins <= limit ->
                  let mins' = if isNeg then negate mins else mins
                  in Right ((dt, TimezoneOffset mins'), rest)
              | otherwise ->
                let sign = if isNeg then "-" else "+"
                in Left ("offset invalid, got: " <> sign <> show mins)
  processOne acc Format_Spaces s = case span isSpace s of
    ("", _) -> Left ("no white space at start of: " <> s)
    (_, s2) -> Right (acc, s2)
  processOne acc (Format_Fct tff) s = timeFormatParse tff acc s

  parseHMSign expectColon acc signChar afterSign =
    case signChar of
      '+' -> parseHM False expectColon afterSign acc
      '-' -> parseHM True expectColon afterSign acc
      _   -> parseHM False expectColon (signChar:afterSign) acc

  parseHM isNeg True (h1:h2:':':m1:m2:xs) acc
    | allDigits [h1,h2,m1,m2] = let tz = toTZ isNeg h1 h2 m1 m2
                                in  Right (modTZ (const tz) acc, xs)
    | otherwise = Left ("non-digit char(s) in: " ++ show [h1,h2,m1,m2])
  parseHM isNeg False (h1:h2:m1:m2:xs) acc
    | allDigits [h1,h2,m1,m2] = let tz = toTZ isNeg h1 h2 m1 m2
                                in  Right (modTZ (const tz) acc, xs)
    | otherwise = Left ("non-digit char(s) in: " ++ show [h1,h2,m1,m2])
  parseHM _ _    _ _ = Left "invalid timezone format"

  toTZ isNeg h1 h2 m1 m2 = TimezoneOffset ((if isNeg then negate else id) minutes)
   where
    minutes = (toInt [h1,h2] * 60) + toInt [m1,m2]

  onSuccess f (Right (v, s')) = Right (f v, s')
  onSuccess _ (Left s)        = Left s

  isNumber :: Num a => String -> Either String (a, String)
  isNumber s =
    case span isDigit s of
      ("", s2) -> Left ("no digits at start of:" ++ s2)
      (s1, s2) -> Right (toInt s1, s2)

  getNDigitNum :: Int -> String -> Either String (Int64, String)
  getNDigitNum n s =
    case getNChar n s of
      Left err                            -> Left err
      Right (s1, s2)
        | not (allDigits s1) -> Left ("non-digit char(s) in " ++ show s1)
        | otherwise          -> Right (toInt s1, s2)

  getMonth :: String -> Either String (Month, String)
  getMonth s =
    getNChar 3 s >>= \(s1, s2) -> monthFromShort s1 >>= \m -> Right (m, s2)

  getNChar :: Int -> String -> Either String (String, String)
  getNChar n s
    | length s1 < n =
        Left ("not enough chars: expecting " ++ show n ++ " got " ++ show s1)
    | otherwise = Right (s1, s2)
   where
    (s1, s2) = splitAt n s

  toInt :: Num a => String -> a
  toInt = foldl (\acc w -> acc * 10 + fromIntegral (ord w - ord '0')) 0

  allDigits = all isDigit

  ini = (DateTime (Date 0 (toEnum 0) 0) (TimeOfDay 0 0 0 0), TimezoneOffset 0)

  modDT   f (dt, tz) = (f dt, tz)
  modDate f (DateTime d tp, tz) = (DateTime (f d) tp, tz)
  modTime f (DateTime d tp, tz) = (DateTime d (f tp), tz)
  modTZ   f (dt, tz) = (dt, f tz)

  setYear :: Int64 -> Date -> Date
  setYear  y (Date _ m d) = Date (fromIntegral y) m d
  setMonth m (Date y _ d) = Date y m d
  setDay   d (Date y m _) = Date y m (fromIntegral d)
  setHour  h (TimeOfDay _ m s ns) = TimeOfDay (Hours h) m s ns
  setMin   m (TimeOfDay h _ s ns) = TimeOfDay h (Minutes m) s ns
  setSec   s (TimeOfDay h m _ ns) = TimeOfDay h m (Seconds s) ns
  setNS    v (TimeOfDay h m s _ ) = TimeOfDay h m s (NanoSeconds v)

  setNsMask :: (Int, Int) -> Int64 -> TimeOfDay -> TimeOfDay
  setNsMask (shift, mask) val (TimeOfDay h mins seconds (NanoSeconds ns)) =
    let (nsD,keepL) = ns `divMod` s
        (keepH,_)   = nsD `divMod` m
        v           = ((keepH * m + fromIntegral val) * s) + keepL
    in  TimeOfDay h mins seconds (NanoSeconds v)
   where
    s = 10 ^ shift
    m = 10 ^ mask

-- | Like 'localTimeParseE', but with simpler handing of failure. Does not yield
-- the remaining unparsed string on success.
--
-- On failure, returns 'Nothing'. If successful, yields 'Just' the parsed value.
localTimeParse ::
     TimeFormat format
  => format -- ^ The format to use for parsing.
  -> String -- ^ The string to parse.
  -> Maybe (LocalTime DateTime)
localTimeParse fmt s =
  either (const Nothing) (Just . fst) $ localTimeParseE fmt s

-- | Like 'localTimeParseE' but the time value is automatically converted to
-- global time.
timeParseE ::
     TimeFormat format
  => format
  -> String
  -> Either (TimeFormatElem, String) (DateTime, String)
timeParseE fmt timeString =
  (\(d, s) -> Right (localTimeToGlobal d, s)) =<< localTimeParseE fmt timeString

-- | Like 'localTimeParse' but the time value is automatically converted to
-- global time.
timeParse :: TimeFormat format => format -> String -> Maybe DateTime
timeParse fmt s = localTimeToGlobal `fmap` localTimeParse fmt s
