{-# LANGUAGE OverloadedStrings #-}

module TextZipperTest (testZipper) where

import Data.ByteString
import Day3.TextZipper
import Pipes
import Test.HUnit

testZipper :: Test
testZipper = TestList [testMove, testMoveUpAndDown]

testMove :: Test
testMove = TestCase $ do
  zipper <- textZipper schematic
  assertEqual
    "initial cursor position is (1,1)"
    (1, 1)
    (currentPosition zipper)
  assertEqual
    "first character is '4'"
    (Just '4')
    (currentChar zipper)
  moved <- moveDown zipper
  assertEqual
    "cursor position is (2,1) after being moved down"
    (2, 1)
    (currentPosition moved)
  assertEqual
    "the first character in the line below is '.'"
    (Just '.')
    (currentChar moved)

testMoveUpAndDown :: Test
testMoveUpAndDown = TestCase $ do
  zipper <- textZipper schematic >>= moveDown
  unmoved <- moveDown $ moveUp zipper
  assertEqual
    "cursor is back to the same character"
    (currentChar zipper)
    (currentChar unmoved)

schematic :: Producer ByteString IO ()
schematic = each $ group schematicStr

schematicStr :: ByteString
schematicStr =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598.."
