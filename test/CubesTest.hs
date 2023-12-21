{-# LANGUAGE OverloadedStrings #-}

module CubesTest (testCubes) where

import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import Data.ByteString
import Day2.Cubes
import Test.HUnit

testCubes :: Test
testCubes =
  TestList
    [ "parse games" ~: parseGames,
      "game equality" ~: gameEquality
    ]

parseGames :: Test
parseGames =
  TestList
    [ "no cubes" ~: parseNoCubes,
      "game 1" ~: parseGame1,
      "game 2" ~: parseGame2,
      "game 3" ~: parseGame3,
      "game 4" ~: parseGame4,
      "game 5" ~: parseGame5
    ]

gameEquality :: Test
gameEquality =
  TestList
    ["no cubes equal if same id" ~: noCubesEqual]

parseNoCubes :: Test
parseNoCubes = Right (Game 0 [CubeSelection []]) ~=? parseGame "Game 0: "

parseGame1 :: Test
parseGame1 =
  Right
    ( Game
        1
        [ CubeSelection [(Blue, 3), (Red, 4)],
          CubeSelection [(Red, 1), (Green, 2), (Blue, 6)],
          CubeSelection [(Green, 2)]
        ]
    )
    ~=? parseGame game1

parseGame2 :: Test
parseGame2 =
  Right
    ( Game
        2
        [ CubeSelection [(Blue, 1), (Green, 2)],
          CubeSelection [(Green, 3), (Blue, 4), (Red, 1)],
          CubeSelection [(Green, 1), (Blue, 1)]
        ]
    )
    ~=? parseGame game2

parseGame3 :: Test
parseGame3 =
  Right
    ( Game
        3
        [ CubeSelection [(Green, 8), (Blue, 6), (Red, 20)],
          CubeSelection [(Blue, 5), (Red, 4), (Green, 13)],
          CubeSelection [(Green, 5), (Red, 1)]
        ]
    )
    ~=? parseGame game3

parseGame4 :: Test
parseGame4 =
  Right
    ( Game
        4
        [ CubeSelection [(Green, 1), (Red, 3), (Blue, 6)],
          CubeSelection [(Green, 3), (Red, 6)],
          CubeSelection [(Green, 3), (Blue, 15), (Red, 14)]
        ]
    )
    ~=? parseGame game4

parseGame5 :: Test
parseGame5 =
  Right
    ( Game
        5
        [ CubeSelection [(Red, 6), (Blue, 1), (Green, 3)],
          CubeSelection [(Blue, 2), (Red, 1), (Green, 2)]
        ]
    )
    ~=? parseGame game5

noCubesEqual :: Test
noCubesEqual = Game 0 [CubeSelection []] ~=? Game 0 [CubeSelection [(Red, 0), (Green, 0), (Blue, 0)]]

parseGame :: ByteString -> Either String Game
parseGame = parseOnly $ gameParser <* endOfInput

game1 :: ByteString
game1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

game2 :: ByteString
game2 = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"

game3 :: ByteString
game3 = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

game4 :: ByteString
game4 = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"

game5 :: ByteString
game5 = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
