{-# LANGUAGE OverloadedStrings #-}
-- Heavily inspired by https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell-2/
module Day3.TextZipper
  ( TextZipper (..),
    Location,
    textZipper,
    moveDown,
    moveUp,
    currentPosition,
    currentChar,
  )
where

import Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.ByteString.Char8 as BS
import Pipes
import qualified Pipes.Attoparsec as AttoparsecPipe
import Prelude as P

-- | A Zipper that can move two-dimensionally along
-- | a stream of newline-separated text
data TextZipper m a = TextZipper
  { tzLeft :: a,
    tzRight :: a,
    tzAbove :: [a],
    tzBelow :: Producer a m ()
  }

type Location = (Int, Int)

-- | Create a text zipper from a stream of bytes.
textZipper :: (Monad m) => Producer ByteString m () -> m (TextZipper m ByteString)
textZipper src = do
  line <- next $ lineParser src
  case line of
    Left _ -> return $ TextZipper "" "" [] (return ())
    Right (text, rest) -> return $ TextZipper "" text [] rest

-- | Move a text zipper down a row, remaining at the same column if possible.
moveDown :: (Monad m) => TextZipper m ByteString -> m (TextZipper m ByteString)
moveDown zipper = do
  line <- next $ tzBelow zipper
  case line of
    Left _ -> return zipper
    Right (text, rest) ->
      return $
        let (left, right) = splitLineAtSameColumn zipper text
            above = (tzAbove zipper <> [currentLine zipper])
         in TextZipper left right above rest

-- | Move a text zipper up a row, remaining at the same column if possible.
moveUp :: (Monad m) => TextZipper m ByteString -> TextZipper m ByteString
moveUp zipper
  | not $ P.null (tzAbove zipper) =
      let text = P.head $ tzAbove zipper
          (left, right) = splitLineAtSameColumn zipper text
          above = P.tail $ tzAbove zipper
          below = yield (currentLine zipper) <> tzBelow zipper
       in zipper
            { tzAbove = above,
              tzBelow = below,
              tzLeft = left,
              tzRight = right
            }
  | otherwise = zipper

-- | Split a bytestring at the same column the given zipper is located,
-- | if the bytestring is long enough.
splitLineAtSameColumn :: TextZipper m ByteString -> ByteString -> (ByteString, ByteString)
splitLineAtSameColumn zipper = BS.splitAt (BS.length $ tzLeft zipper)

-- | Access the whole line at the zipper's location
currentLine :: (Monoid a) => TextZipper m a -> a
currentLine zipper = tzLeft zipper <> tzRight zipper

-- | Split the given producer at newlines, creating a new producer that yields
-- | just the lines, without newline characters.
lineParser :: (Monad m) => Producer ByteString m () -> Producer ByteString m ()
lineParser src = void lineProducer
  where
    lineProducer = AttoparsecPipe.parsed parseL src
    parseL = Attoparsec.takeWhile (not . isNewline) <* skipWhile isNewline
    isNewline c = c == '\r' || c == '\n'

-- | Get the row and column of the zipper's current position
currentPosition :: TextZipper m ByteString -> Location
currentPosition zipper =
  (P.length (tzAbove zipper) + 1, BS.length (tzLeft zipper) + 1)

-- | Access the character at the cursor position, if one is available.
currentChar :: TextZipper m ByteString -> Maybe Char
currentChar zipper = case BS.uncons (tzRight zipper) of
  Nothing -> Nothing
  Just (c, _) -> Just c
