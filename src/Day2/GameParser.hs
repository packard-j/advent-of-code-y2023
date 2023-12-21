{-# LANGUAGE OverloadedStrings #-}

module Day2.GameParser (gameParser) where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, sepBy, string)
import Data.Attoparsec.ByteString.Char8 (decimal, space)
import Day2.Game

gameParser :: Parser Game
gameParser = do
  gid <- string "Game " >> decimal <* ": "
  selections <- selectionParser `sepBy` string "; "
  return $ Game gid selections

selectionParser :: Parser CubeSelection
selectionParser = CubeSelection <$> colorCount `sepBy` string ", "
  where
    colorCount = flip (,) <$> (decimal <* space) <*> colorParser

colorParser :: Parser Color
colorParser =
  (string "red" >> pure Red)
    <|> (string "green" >> pure Green)
    <|> (string "blue" >> pure Blue)
