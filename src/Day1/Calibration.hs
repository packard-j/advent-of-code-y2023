module Day1.Calibration (runCalibration) where

import Data.Attoparsec.ByteString.Char8 (endOfLine)
import Data.ByteString (ByteString)
import Day1.CalibrationParser (calibrationValue)
import Pipes
import Pipes.Attoparsec (parse)
import Pipes.ByteString (stdin)
import Pipes.Parse (runStateT)
import qualified Pipes.Prelude as P

-- | Read calibration values line by line and print their sum
runCalibration :: IO ()
runCalibration = sumCalibration stdin >>= print
  where
    sumCalibration = P.sum . calibrationValues

-- | Create a producer of calibration values from a bytestring producer
calibrationValues :: Producer ByteString IO () -> Producer Int IO ()
calibrationValues src = do
  (r, rest) <- lift $ runStateT calibrationLine src
  case r of
    Just (Right n) -> yield n *> calibrationValues rest
    -- Errors will be handled by terminating the producer
    _ -> return ()
  where
    calibrationLine = parse $ calibrationValue <* endOfLine
