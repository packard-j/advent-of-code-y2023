module Day3.GearRatios () where

import Data.Attoparsec.ByteString.Char8 as Attoparsec
import Day3.TextZipper

import Prelude as P

data Schematic = Schematic
  { symbols :: [Symbol],
    numbers :: [PartNumber]
  }

data Node a = Node
  { unNode :: a,
    span :: Span
  }

data Symbol = Symbol
  { node :: Node Char,
    neighbors :: [PartNumber]
  }

type PartNumber = Node Int


type Span = (Location, Int)


schematicParser :: Parser Schematic
schematicParser = undefined

-- | Producer ByteString m ()
-- | -> TextZipper m ByteString
-- | -> Schematic

{-
467..114.. > 467..114..  > [PartNumber]
...*......   ...*......  |
..35..633.   ..35..633.  +-> [Symbol]
......#...
617*......  (take up to
.....+.58.   3 lines)
..592.....
......755.
...$.*....
.664.598..
 -}
