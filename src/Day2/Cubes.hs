{-# LANGUAGE OverloadedStrings #-}

module Day2.Cubes (Game (..), CubeSelection (..), Color (..), countOf, gameParser, selectionParser, colorParser) where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, sepBy, string)
import Data.Attoparsec.ByteString.Char8 (decimal, space)
import Data.List (intercalate)

data Game = Game GameID [CubeSelection] deriving (Show, Eq)

type GameID = Int

newtype CubeSelection = CubeSelection [(Color, Int)]

instance Eq CubeSelection where
  s == s' = all (\c -> countOf s c == countOf s' c) colors

instance Show CubeSelection where
  show s = intercalate ", " ((\c -> showCount c (countOf s c)) <$> colors)
    where
      showCount color count = show color ++ ": " ++ show count

countOf :: CubeSelection -> Color -> Int
countOf (CubeSelection counts) color = sum $ snd <$> filter ((== color) . fst) counts

data Color = Red | Green | Blue deriving (Show, Eq, Enum)

colors :: [Color]
colors = enumFrom Red

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
