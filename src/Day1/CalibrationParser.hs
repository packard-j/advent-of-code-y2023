module Day1.CalibrationParser (calibrationValue) where
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (many1, skipMany, digit, letter_ascii)

-- | Parse a calibration value, which "can be found by combining
-- | the first digit and the last digit (in that order) to form a
-- | single two-digit number."
calibrationValue :: Parser Int
calibrationValue = do
  digits <- many1 parseDigit
  return $ read [head digits, last digits]

-- | Parse a digit character surrounded by zero or more letters
parseDigit :: Parser Char
parseDigit = skipLetters >> digit <* skipLetters where
  skipLetters = skipMany letter_ascii

