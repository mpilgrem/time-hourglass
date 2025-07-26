module CustomTimeFormatFct
  ( customTimeFormatFct
  ) where

import           Data.Char ( isDigit, ord )
import           Data.Hourglass
                   ( Date (..), DateTime (..), Month (..), Period (..)
                   ,TimeFormatFct (..), dateAddPeriod, getDayOfTheYear
                   )
import           Data.Int ( Int64 )

-- | To test 'Format_Fct', we provide a custom time format function that will
-- replicate @Format_DayYear3@.
customTimeFormatFct :: TimeFormatFct
customTimeFormatFct = TimeFormatFct
  { timeFormatFctName = "customTimeFormatFct"
  , timeFormatParse = customTimeFormatParse
  , timeFormatPrint = customTimeFormatPrint
  }
 where
  customTimeFormatParse acc s =
    let y = (dateYear . dtDate . fst) acc
        result = getNDigitNum 3 s
    in  case result of
          Left err -> Left err
          Right (d, s')
              -- We can't be more helpful because we may not yet know the
              -- intended year
            | d >= 1 && d <= 366 ->
                let p = Period 0 0 (fromIntegral d - 1)
                    startOfYear = Date y January 1
                in  Right (modDate (const (dateAddPeriod startOfYear p)) acc, s')
            | otherwise -> Left ("day of year invalid, got: " <> show d)

  customTimeFormatPrint date _ = pad3 (getDayOfTheYear $ dtDate date)

  getNDigitNum :: Int -> String -> Either String (Int64, String)
  getNDigitNum n s =
    case getNChar n s of
      Left err -> Left err
      Right (s1, s2)
        | not (allDigits s1) -> Left ("non-digit char(s) in " ++ show s1)
        | otherwise          -> Right (toInt s1, s2)

  getNChar :: Int -> String -> Either String (String, String)
  getNChar n s
    | length s1 < n =
        Left ("not enough chars: expecting " ++ show n ++ " got " ++ show s1)
    | otherwise = Right (s1, s2)
   where
    (s1, s2) = splitAt n s

  toInt :: Num a => String -> a
  toInt = foldl (\acc w -> acc * 10 + fromIntegral (ord w - ord '0')) 0

  -- | Pad a number to 3 digits.
  pad3 :: (Show a, Ord a, Num a, Integral a) => a -> String
  pad3 v
    | v >= 100  = show v
    | v >= 10   = '0' : show v
    | otherwise = '0':'0': show v

  modDate f (DateTime d tp, tz) = (DateTime (f d) tp, tz)

  allDigits = all isDigit
