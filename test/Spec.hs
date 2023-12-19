import Test.HUnit
import CalibrationParserTest

tests :: Test
tests = TestList
  [ "Day 1" ~: testCalibrationValue ]


main :: IO ()
main = do
  _ <- runTestTT tests
  return ()
