{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import Common
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

type InputRaces = ([Int], [Int])

type Races = [(Int, Int)]

parser :: Parser InputRaces
parser = (,) <$> ("Time:" *> space *> L.decimal `sepEndBy` some " " <* eol) <*> ("Distance:" *> space *> L.decimal `sepEndBy` some " ")

getRaces :: InputRaces -> Races
getRaces = gr []
  where
    gr buff ([], []) = buff
    gr buff (t : ts, d : ds) = gr ((t, d) : buff) (ts, ds)

getPossibleSpeeds :: Races -> [[Int]]
getPossibleSpeeds = map (\(t, d) -> [x | x <- [0 .. t], x * (t - x) > d])

solve1 = product . map length . getPossibleSpeeds . getRaces

joinInt :: [Int] -> Int
joinInt = read . concatMap show

getOneRace :: InputRaces -> (Int, Int)
getOneRace (t, d) = (joinInt t, joinInt d)

solve2 = sum . map length . getPossibleSpeeds . singleton . getOneRace

day6 :: Difficulty -> Problem InputRaces Int
day6 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . solve1
        Hard -> Just . solve2,
      printOutput = show
    }
