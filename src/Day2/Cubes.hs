module Day2.Cubes () where

import Day2.Game
import Day2.GameParser

possibleGameIDSum :: CubeSelection -> [Game] -> Int
possibleGameIDSum available games = sum $ gameID <$> filter (`isGamePossible` available) games
