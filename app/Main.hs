module Main (main) where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Day1.Calibration
import Day2.Cubes
import System.Environment

programs :: [IO ()]
programs =
  [ runCalibration,
    runPossibleGameIDSum
  ]

-- Run one of the advent of code programs based on the first command line argument,
-- defaulting to day 1 if unspecified.
main :: IO ()
main = do
  args <- getArgs
  let day = if null args then "1" else head args
   in case parseOnly (dayParser <* endOfInput) (pack day) of
        (Left err) -> putStrLn err
        (Right d) -> programs !! (d - 1)

-- Parse a number in the range [1, <number of days of advent of code I've completed>]
dayParser :: Parser Int
dayParser = do
  day <- decimal
  if day >= minDay && day <= maxDay
    then return day
    else fail (show day ++ "is out of range: [" ++ show minDay ++ ", " ++ show maxDay ++ "]")
  where
    minDay = 1
    maxDay = length programs

