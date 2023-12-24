{-# LANGUAGE OverloadedStrings #-}

module CubesTest (testCubes) where

import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import Data.ByteString
import Day2.Cubes
import Day2.Game
import Day2.GameParser
import Test.HUnit

testCubes :: Test
testCubes =
  TestList
    [ "parse games" ~: parseGames,
      "game equality" ~: gameEquality,
      "possible games" ~: testPossibleGames
    ]

parseGames :: Test
parseGames =
  TestList
    [ "no cubes" ~: parseNoCubes,
      "game 1" ~: testParse game1 game1_str,
      "game 2" ~: testParse game2 game2_str,
      "game 3" ~: testParse game3 game3_str,
      "game 4" ~: testParse game4 game4_str,
      "game 5" ~: testParse game5 game5_str
    ]

gameEquality :: Test
gameEquality =
  TestList
    ["no cubes equal if same id" ~: noCubesEqual]

testPossibleGames :: Test
testPossibleGames =
  [game1, game2, game5]
    ~=? possibleGames
      (CubeSelection [(Red, 12), (Green, 13), (Blue, 14)])
      [game1, game2, game3, game4, game5]

testParse :: Game -> ByteString -> Test
testParse game str = Right game ~=? parseGame str

parseNoCubes :: Test
parseNoCubes = Right (Game 0 [CubeSelection []]) ~=? parseGame "Game 0: "

noCubesEqual :: Test
noCubesEqual = Game 0 [CubeSelection []] ~=? Game 0 [CubeSelection [(Red, 0), (Green, 0), (Blue, 0)]]

parseGame :: ByteString -> Either String Game
parseGame = parseOnly $ gameParser <* endOfInput

game1_str :: ByteString
game1_str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

game1 :: Game
game1 =
  Game
    1
    [ CubeSelection [(Blue, 3), (Red, 4)],
      CubeSelection [(Red, 1), (Green, 2), (Blue, 6)],
      CubeSelection [(Green, 2)]
    ]

game2_str :: ByteString
game2_str = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"

game2 :: Game
game2 =
  Game
    2
    [ CubeSelection [(Blue, 1), (Green, 2)],
      CubeSelection [(Green, 3), (Blue, 4), (Red, 1)],
      CubeSelection [(Green, 1), (Blue, 1)]
    ]

game3_str :: ByteString
game3_str = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

game3 :: Game
game3 =
  Game
    3
    [ CubeSelection [(Green, 8), (Blue, 6), (Red, 20)],
      CubeSelection [(Blue, 5), (Red, 4), (Green, 13)],
      CubeSelection [(Green, 5), (Red, 1)]
    ]

game4_str :: ByteString
game4_str = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"

game4 :: Game
game4 =
  Game
    4
    [ CubeSelection [(Green, 1), (Red, 3), (Blue, 6)],
      CubeSelection [(Green, 3), (Red, 6)],
      CubeSelection [(Green, 3), (Blue, 15), (Red, 14)]
    ]

game5_str :: ByteString
game5_str = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

game5 :: Game
game5 =
  Game
    5
    [ CubeSelection [(Red, 6), (Blue, 1), (Green, 3)],
      CubeSelection [(Blue, 2), (Red, 1), (Green, 2)]
    ]
