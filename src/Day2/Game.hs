module Day2.Game
  ( Game (..),
    GameID,
    CubeSelection (..),
    Color (..),
    gameID,
    countOf,
    isGamePossible,
  )
where

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

data Color = Red | Green | Blue deriving (Show, Eq, Enum)

gameID :: Game -> GameID
gameID (Game gid _) = gid

countOf :: CubeSelection -> Color -> Int
countOf (CubeSelection counts) color = sum $ snd <$> filter ((== color) . fst) counts

isGamePossible :: Game -> CubeSelection -> Bool
isGamePossible (Game _ selections) available =
  let maximums = CubeSelection $ (\c -> (c, maxSelection selections c)) <$> colors
   in all (\c -> countOf maximums c <= countOf available c) colors

maxSelection :: [CubeSelection] -> Color -> Int
maxSelection [] _ = 0
maxSelection selections color = maximum $ (`countOf` color) <$> selections

colors :: [Color]
colors = enumFrom Red
