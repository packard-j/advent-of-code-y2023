{-# LANGUAGE OverloadedStrings #-}

module Day2.GameParser (gameParser, selectionParser) where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, sepBy, string)
import Data.Attoparsec.ByteString.Char8 (decimal, space)
import Day2.Game

-- | Parses a game in the form:
-- | Game <id>: <color> <int>, <color> <int>; <color <int> ...
gameParser :: Parser Game
gameParser = do
  gid <- string "Game " >> decimal <* ": "
  selections <- selectionParser `sepBy` string "; "
  return $ Game gid selections

-- | Parses a cube selection in the form:
-- | <color> <int>, <color> <int> ...
selectionParser :: Parser CubeSelection
selectionParser = CubeSelection <$> colorCount `sepBy` string ", "
  where
    colorCount = flip (,) <$> (decimal <* space) <*> colorParser

-- | Parses a cube color, which is one of:
-- | "red" | "green" | "blue"
colorParser :: Parser Color
colorParser =
  (string "red" >> pure Red)
    <|> (string "green" >> pure Green)
    <|> (string "blue" >> pure Blue)
