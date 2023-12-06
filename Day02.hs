{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import Common
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

data Color = R | G | B deriving (Eq, Show)

data Cubes = Cubes {number :: Integer, color :: Color} deriving (Eq, Show)

type Turn = [Cubes]

type Counts = (Integer, Integer, Integer)

data Game = Game {gid :: Integer, turns :: [Turn]} deriving (Eq, Show)

parseCube :: Parser Cubes
parseCube = Cubes <$> (skipMany spaceChar *> L.decimal <* spaceChar) <*> choice [R <$ "red", G <$ "green", B <$ "blue"]

parseTurn :: Parser Turn
parseTurn = parseCube `sepEndBy` ","

parseGame :: Parser Game
parseGame = Game <$> ("Game" *> spaceChar *> L.decimal <* ":" <* spaceChar) <*> (parseTurn `sepEndBy` ";")

processTurn :: [Cubes] -> Counts
processTurn =
  foldl
    ( \(r, g, b) c ->
        case color c of
          R -> (r + number c, g, b)
          G -> (r, g + number c, b)
          B -> (r, g, b + number c)
    )
    (0, 0, 0)

maxCountsGame :: [Turn] -> Counts
maxCountsGame = foldl (\(r, g, b) (x, y, z) -> (max r x, max g y, max b z)) (0, 0, 0) . map processTurn

compareCounts :: (Integer -> Integer -> Bool) -> Counts -> Counts -> Bool
compareCounts fn (x1, x2, x3) (y1, y2, y3) = fn x1 y1 && fn x2 y2 && fn x3 y3

filterPossibleGames :: Counts -> [Game] -> [Game]
filterPossibleGames maxCounts = filter (compareCounts (>=) maxCounts . maxCountsGame . turns)

countsPower :: Counts -> Integer
countsPower (r, g, b) = r * g * b

day2 :: Difficulty -> Problem [Game] Integer
day2 diff =
  Problem
    { parseInput = eitherToMaybe . parse (parseGame `sepEndBy` eol) "",
      solve = case diff of
        Easy -> Just . sum . map gid . filterPossibleGames (12, 13, 14)
        Hard -> Just . sum . map (countsPower . maxCountsGame . turns),
      printOutput = show
    }
