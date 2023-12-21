{-# LANGUAGE OverloadedStrings #-}

module CalibrationParserTest (testCalibrationValue) where

import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import Data.ByteString (ByteString)
import Day1.CalibrationParser
import Test.HUnit

testCalibrationValue :: Test
testCalibrationValue =
  TestList
    [ "1abc2" ~: ex1,
      "pqr3stu8vwx" ~: ex2,
      "a1b2c3d4e5f" ~: ex3,
      "treb7uchet" ~: ex4
    ]

parseCalibrationValue :: ByteString -> Either String Int
parseCalibrationValue = parseOnly $ calibrationValue <* endOfInput

ex1 :: Test
ex1 = Right 12 ~=? parseCalibrationValue "1abc2"

ex2 :: Test
ex2 = Right 38 ~=? parseCalibrationValue "pqr3stu8vwx"

ex3 :: Test
ex3 = Right 15 ~=? parseCalibrationValue "a1b2c3d4e5f"

ex4 :: Test
ex4 = Right 77 ~=? parseCalibrationValue "treb7uchet"
