import Test.HUnit
import CalibrationParserTest
import CubesTest

tests :: Test
tests = TestList
  [ "Day 1" ~: testCalibrationValue,
    "Day 2" ~: testCubes ]


main :: IO ()
main = do
  _ <- runTestTT tests
  return ()
