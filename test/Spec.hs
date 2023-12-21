import CalibrationParserTest
import CubesTest
import Test.HUnit

tests :: Test
tests =
  TestList
    [ "Day 1" ~: testCalibrationValue,
      "Day 2" ~: testCubes
    ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()
