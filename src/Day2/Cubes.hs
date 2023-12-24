{-# LANGUAGE ApplicativeDo #-}

module Day2.Cubes
  ( runPossibleGameIDSum,
    possibleGameIDSum,
    possibleGames
  )
where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (endOfLine, many1)
import Data.ByteString (ByteString)
import Day2.Game
import Day2.GameParser
import Pipes
import Pipes.Attoparsec (parsed)
import Pipes.ByteString (stdin)

-- | Read a set of games and available cubes and print the sum of the
-- | ids of the games that are possible with the available cubes.
runPossibleGameIDSum :: IO ()
runPossibleGameIDSum = do
  r <- runEffect $ parseOne gamesAndCubesParser stdin
  case r of
    Just (games, cubes) -> print $ possibleGameIDSum cubes games
    Nothing -> putStrLn "couldn't read your input"

-- | Compute the sum of the ids of games that are possible with the given
-- | cube selection.
possibleGameIDSum :: CubeSelection -> [Game] -> Int
possibleGameIDSum available games = sum $ gameID <$> possibleGames available games

-- | Filter to only the games that are possible with the given cube selection.
possibleGames :: CubeSelection -> [Game] -> [Game]
possibleGames available = filter (`isGamePossible` available)

-- | Parse a single value from the given bytestring stream, or return Nothing.
parseOne :: Monad m => Parser b -> Producer ByteString m () -> Effect m (Maybe b)
parseOne parser src = do
  r <- lift . next $ parsed parser src
  case r of
    Left _ -> return Nothing
    Right (first, _) -> return $ Just first

-- | Parse a list of games seprated by newlines followed by a cube selection.
gamesAndCubesParser :: Parser ([Game], CubeSelection)
gamesAndCubesParser = do
  games <- many1 (gameParser <* endOfLine)
  available <- selectionParser
  return (games, available)
