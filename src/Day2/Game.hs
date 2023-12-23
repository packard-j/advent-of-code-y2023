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

-- | Represents a game where colored cubes are repeatedly selected at random from a bag
data Game = Game GameID [CubeSelection] deriving (Show, Eq)
type GameID = Int

-- | A selection of cubes of different colors, represented by an association list
-- | of colors to number of cubes of that color
newtype CubeSelection = CubeSelection [(Color, Int)]

instance Eq CubeSelection where
  s == s' = all (\c -> countOf s c == countOf s' c) colors

instance Show CubeSelection where
  show s = intercalate ", " ((\c -> showCount c (countOf s c)) <$> colors)
    where
      showCount color count = show color ++ ": " ++ show count

-- | Represents the color of a cube
data Color = Red | Green | Blue deriving (Show, Eq, Enum)

-- | Access the ID of a game.
gameID :: Game -> GameID
gameID (Game gid _) = gid

-- | The number of cubes of the given color in the selection.
countOf :: CubeSelection -> Color -> Int
countOf (CubeSelection counts) color = sum $ snd <$> filter ((== color) . fst) counts

-- | Does the given selection have enough cubes of each color for the game
-- | to have been played with it?
isGamePossible :: Game -> CubeSelection -> Bool
isGamePossible (Game _ selections) available =
  let maximums = maxSelection selections
   in all (\c -> countOf maximums c <= countOf available c) colors

-- | What was the maximum count of cubes in each color across the given selections?
maxSelection :: [CubeSelection] -> CubeSelection
maxSelection selections = CubeSelection $ (\c -> (c, maxOfColor c)) <$> colors where
  maxOfColor c = maximum $ (`countOf` c) <$> CubeSelection []:selections

colors :: [Color]
colors = enumFrom Red
