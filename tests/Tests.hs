{-# OPTIONS_GHC -Wno-orphans            #-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import qualified Control.Exception as E
import           Control.Monad ( when )
import           Data.Hourglass
                   ( Date (..), DateTime (..), Duration (..), Elapsed (..)
                   , ElapsedP (..), Hours (..), ISO8601_Date (..)
                   , ISO8601_DateAndTime (..), LocalTime, Minutes (..)
                   , Month (..), NanoSeconds (..), Period (..), Seconds (..)
                   , Time, TimeFormat (..), TimeOfDay (..), TimezoneOffset (..)
                   , WeekDay, dateAddPeriod, daysInMonth, getWeekDay, localTime
                   , localTimeFromGlobal, localTimeGetTimezone, localTimeParseE
                   , localTimeSetTimezone, localTimeToGlobal, timeConvert
                   , timeGetDateTimeOfDay, timeGetElapsed, timeParseE, timePrint
                   )
import           Data.Int ( Int64 )
import           Data.Ratio ( (%) )
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.POSIX as T
import qualified Data.Time.Format as T
import           Test.Tasty ( TestTree, defaultMain, testGroup )
import           Test.Tasty.HUnit
                   ( Assertion, (@=?), assertEqual, assertFailure, testCase )
import           Test.Tasty.QuickCheck
                   ( Arbitrary (..), choose, elements, testProperty )
import           Time.Epoch
                   ( ElapsedSince (..), ElapsedSinceP (..), UnixEpoch
                   , WindowsEpoch
                   )
import           TimeDB ( parseTimeConv )
import           TimeRange ( dateRange, hiElapsed, loElapsed )

elapsedToPosixTime :: Elapsed -> T.POSIXTime
elapsedToPosixTime (Elapsed (Seconds s)) = fromIntegral s

dateEqual :: LocalTime DateTime -> T.UTCTime -> Bool
dateEqual localtime utcTime = and
  [ fromIntegral y == y', m' == (fromEnum m + 1), d' == d
  , fromIntegral h' == h, fromIntegral mi' == mi, sec' == sec
  ]
 where
  (y',m',d') = T.toGregorian (T.utctDay utcTime)
  daytime    = floor $ T.utctDayTime utcTime
  (dt', sec')= daytime `divMod` 60
  (h' , mi') = dt' `divMod` 60
  (DateTime (Date y m d) (TimeOfDay h mi sec _)) = localTimeToGlobal localtime

-- | The @Date@ type is able to represent some values that aren't actually legal,
-- specifically dates with a day field outside of the range of dates in the
-- month. This function validates a @Date@.
isValidDate :: Date -> Bool
isValidDate (Date y m d) = d > 0 && d <= daysInMonth y m

instance Arbitrary Seconds where
  arbitrary = Seconds . toHiLo <$> arbitrary
   where
    toHiLo v
      | v > loElapsed && v < hiElapsed = v
      | v > hiElapsed = v `mod` hiElapsed
      | v < loElapsed = v `mod` loElapsed
      | otherwise = error "internal error"

instance Arbitrary Minutes where
  arbitrary = Minutes <$> choose (-1125899906842624, 1125899906842624)

instance Arbitrary Hours where
  arbitrary = Hours <$> choose (-1125899906842, 1125899906842)

instance Arbitrary NanoSeconds where
  arbitrary = NanoSeconds <$> choose (0, 100000000)

instance Arbitrary Elapsed where
  arbitrary = Elapsed <$> arbitrary

instance Arbitrary TimezoneOffset where
  arbitrary = TimezoneOffset <$> choose (-(11*60), 11*60)

instance Arbitrary Duration where
  arbitrary = Duration <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Period where
  arbitrary = Period
    <$> choose (-29,29)
    <*> choose (-27,27)
    <*> choose (-400,400)

instance Arbitrary Month where
  arbitrary = elements [January ..]

instance Arbitrary DateTime where
  arbitrary = DateTime <$> arbitrary <*> arbitrary

instance Arbitrary Date where
  arbitrary = do
    year <- choose dateRange
    month <- arbitrary
    Date year month <$> choose (1, daysInMonth year month)

instance Arbitrary TimeOfDay where
  arbitrary = (TimeOfDay . Hours <$> choose (0, 23))
    <*> (Minutes <$> choose (0,59))
    <*> (Seconds <$> choose (0,59))
    <*> arbitrary

instance (Time t, Arbitrary t) => Arbitrary (LocalTime t) where
  arbitrary = localTime <$> arbitrary <*> arbitrary

eq :: (Eq a, Show a) => a -> a -> Bool
eq expected got
  | expected == got = True
  | otherwise =
      error ("expected: " ++ show expected ++ " got: " ++ show got)

testCaseWith ::
     (Eq a, Num a, Show a)
  => String
  -> (a -> a -> a)
  -> (a, a, a)
  -> TestTree
testCaseWith what fun (x, y, ref) = testCase
  (show x ++ " " ++ what ++ " " ++ show y ++ " ?= " ++ show ref)
  checkAdd
 where
  checkAdd :: Assertion
  checkAdd =
    when (fun x y /= ref) $
      assertFailure $ show (fun x y) ++ " /= " ++ show ref

arithmeticTestAddRef :: [(ElapsedP, ElapsedP, ElapsedP)]
arithmeticTestAddRef = map testRefToElapsedP
  [ ((1, 090000000), (2, 090000000), (3, 180000000))
  , ((1, 900000000), (1, 200000000), (3, 100000000))
  , ((1, 000000001), (0, 999999999), (2, 000000000))
  ]

arithmeticTestSubRef :: [(ElapsedP, ElapsedP, ElapsedP)]
arithmeticTestSubRef = map testRefToElapsedP
  [ ((1, ms 100), (1, ms 100), (0, ms 000))
  , ((1, ms 900), (1, ms 100), (0, ms 800))
  , ((1, ms 100), (0, ms 200), (0, ms 900))
  , ((1, ms 100), (2, ms 400), (-2, ms 700))
  ]
 where
  ms v = v * 1000000

testRefToElapsedP ::
     ((Int64, Int64), (Int64, Int64), (Int64, Int64))
  -> (ElapsedP, ElapsedP, ElapsedP)
testRefToElapsedP (a, b, c) =
  (tupleToElapsedP a, tupleToElapsedP b, tupleToElapsedP c)
 where
  tupleToElapsedP :: (Int64, Int64) -> ElapsedP
  tupleToElapsedP (s, n) = ElapsedP (Elapsed $ Seconds s) (NanoSeconds n)

tests :: [(Elapsed, DateTime, WeekDay, d)] -> TestTree
tests knowns = testGroup "hourglass"
  [ testGroup "known"
      [ testGroup "calendar conv"
          (zipWith (curry toCalendarTest) eint (map tuple12 knowns))
      , testGroup "seconds conv"
          (zipWith (curry toSecondTest) eint (map tuple12 knowns))
      , testGroup "weekday"
          (zipWith (curry toWeekDayTest) eint (map tuple13 knowns))
      ]
  , testGroup "conversion"
      [ testProperty "calendar" $ \(e :: Elapsed) ->
           e `eq` timeGetElapsed (timeGetDateTimeOfDay e)
      , testProperty "win epoch" $ \(e :: Elapsed) ->
          let e2 = timeConvert e :: ElapsedSince WindowsEpoch
          in       timePrint ISO8601_DateAndTime e
              `eq` timePrint ISO8601_DateAndTime e2
      ]
  , testGroup "localtime"
      [ testProperty "eq" $ \(l :: LocalTime Elapsed) ->
          let g = localTimeToGlobal l
          in      l
             `eq` localTimeSetTimezone
                    (localTimeGetTimezone l)
                    (localTimeFromGlobal g)
      , testProperty "set" $ \(l :: LocalTime Elapsed, newTz) ->
          let l2 = localTimeSetTimezone newTz l
          in  localTimeToGlobal l `eq` localTimeToGlobal l2
      ]
  , testGroup "arithmetic"
      [ testGroup "ElapseP add" $
          map (testCaseWith "+" (+)) arithmeticTestAddRef
      , testGroup "ElapseP sub" $
          map (testCaseWith "-" (-)) arithmeticTestSubRef
        {-testProperty "add-diff" $ \(e :: Elapsed, tdiff) ->
          let d@(TimeDiff _ _ day h mi s _) = tdiff { timeDiffYears  = 0
                                                    , timeDiffMonths = 0
                                                    , timeDiffNs     = 0
                                                    }
              i64     = fromIntegral
              accSecs = (((i64 day * 24) + i64 h) * 60 + i64 mi) * 60 + i64 s :: Int64
              e'      = timeAdd e d
           in Seconds accSecs `eq` timeDiff e' e
      , testProperty "calendar-add-month" $ \date@(DateTime (Date y m d) _) ->
          let date'@(DateTime (Date y' m' d') _) = timeAdd date (mempty { timeDiffMonths = 1 })
           in timeGetTimeOfDay date `eq` timeGetTimeOfDay date' &&
              (d `eq` d')                                       &&
              (toEnum ((fromEnum m+1) `mod` 12) `eq` m')        &&
              (if m == December then (y+1) `eq` y' else y `eq` y')
              -}

      -- Make sure our Arbitrary instance only generates valid dates:
      , testProperty "Arbitrary-isValidDate" isValidDate

      , testProperty "dateAddPeriod" (\date period ->
          isValidDate (date `dateAddPeriod` period))
      ]
  , testGroup "formating"
      [ testProperty "iso8601 date" $ \(e :: Elapsed) ->
               calTimeFormatTimeISO8601 (elapsedToPosixTime e)
          `eq` timePrint ISO8601_Date e
      , testProperty "unix seconds" $ \(e :: Elapsed) ->
          let sTime = T.formatTime
                T.defaultTimeLocale
                "%s"
                (T.posixSecondsToUTCTime $ elapsedToPosixTime e)
              sHg = timePrint "EPOCH" e
          in  sTime `eq` sHg
      ]
  , testGroup "parsing"
      [ testProperty "iso8601 date" $ \(e :: Elapsed) ->
          let fmt = calTimeFormatTimeISO8601 (elapsedToPosixTime e)
              ed1  = localTimeParseE ISO8601_Date fmt
              md2  = T.parseTimeM True T.defaultTimeLocale fmt "%F"
          in  case (ed1,md2) of
                (Left err, Nothing) -> error
                  (  "both cannot parse: "
                  ++ show fmt
                  ++ " hourglass-err="
                  ++ show err
                  )
                (Left err, Just _) ->
                  error ("error parsing string: " ++ show err)
                (Right (d1, ""), Just d2) -> dateEqual d1 d2
                (Right (_,_), Nothing) -> True -- let (LocalTime tparsed _) = r in error ("time cannot parse: " ++ show tparsed ++ " " ++ fmt)
                (Right (_, rm), _) ->
                  error ("remaining string after parse: " ++ rm)
      , testProperty "timezone" $ \tz ->
          let r = localTimeParseE "TZHM" (show tz)
          in  case r of
                Right (localtime, "") -> tz `eq` localTimeGetTimezone localtime
                _                     -> error "Cannot parse timezone"
      , testProperty "custom-1" $
          test_property_format ("YYYY-MM-DDTH:MI:S.msusns" :: String)
      , testProperty "custom-2" $
          test_property_format ("Mon DD\\t\\h YYYY at HH\\hMI\\mS\\s.p9\\n\\s" :: String)
      ]
  , testGroup "Regression Tests"
      [ testCase "Real instance of ElapsedP (#33)" $
          let res = toRational (ElapsedP (Elapsed $ Seconds 0) (NanoSeconds 0))
              ref = toRational (0 :: Int) :: Rational
          in  assertEqual "failed equality" ref res
      , testCase "Real instance of ElapsedP (#33) (2)" $
          let res = toRational
                (ElapsedP (Elapsed $ Seconds 100) (NanoSeconds 1000000))
              ref = toRational (100 :: Int) + (1 % 1000) :: Rational
          in  assertEqual "failed equality" ref res
      , testCase "Real instance of ElapsedSinceP, zero ns" $
          let start = ElapsedSince (Seconds 0) :: ElapsedSince UnixEpoch
              startP = ElapsedSinceP start (NanoSeconds 0) :: ElapsedSinceP UnixEpoch
              res = toRational startP
              ref = toRational (0 :: Int) :: Rational
          in  assertEqual "failed equality" ref res
      , testCase "Real instance of ElapsedSinceP, non zero ns" $
          let later = ElapsedSince (Seconds 100) :: ElapsedSince UnixEpoch
              laterP = ElapsedSinceP later (NanoSeconds 1000000) :: ElapsedSinceP UnixEpoch
              res = toRational laterP
              ref = toRational (100 :: Int) + (1 % 1000) :: Rational
          in  assertEqual "failed equality" ref res
      ]
  ]
 where
  toCalendarTest (i, (us, dt)) =
    testCase (show i) (dt @=? timeGetDateTimeOfDay us)
  toSecondTest (i, (us@(Elapsed (Seconds s)), dt)) =
    testCase (show i ++ "-" ++ show s ++ "s") (us @=? timeGetElapsed dt)
  toWeekDayTest (i, (us, wd)) = testCase
    (show i ++ "-" ++ show wd)
    (wd @=? getWeekDay (dtDate $ timeGetDateTimeOfDay us))

  eint :: [Int]
  eint = [1..]

  tuple12 (a,b,_,_) = (a,b)
  tuple13 (a,_,b,_) = (a,b)

  calTimeFormatTimeISO8601 timePosix =
    T.formatTime T.defaultTimeLocale "%F" (T.posixSecondsToUTCTime timePosix)

  test_property_format ::
       (TimeFormat format, Show format)
    => format
    -> DateTime
    -> Bool
  test_property_format fmt dt =
    let p1 = timePrint fmt dt
    in  case timeParseE fmt p1 of
          Left (fmtEl, err) -> error
            (  "cannot decode printed DateTime: "
            ++ show p1
            ++ " with format "
            ++ show fmt
            ++ " error with("
            ++ show fmtEl
            ++ "): " ++ err
            )
          Right (dt2, _) -> dt `eq` dt2

main :: IO ()
main = do
  knowns <- E.catch
    (map parseTimeConv . lines <$> readFile "test-time-db")
    (\(_ :: E.SomeException) -> return [])
  defaultMain (tests knowns)
