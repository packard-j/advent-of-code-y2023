import CalibrationParserTest
import CubesTest
import TextZipperTest
import Test.HUnit

tests :: Test
tests =
  TestList
    [ "Day 1" ~: testCalibrationValue,
      "Day 2" ~: testCubes,
      "Day 3" ~: testZipper
    ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()
