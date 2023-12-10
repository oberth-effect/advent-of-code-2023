{-# LANGUAGE OverloadedStrings #-}

module Day09 where

import Common
import Data.Bifunctor
import Data.Map qualified as M
import Data.Void
import GHC.IO.Buffer (bufferAdd)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.ParserCombinators.Parsec.Char (alphaNum)

type Parser = Parsec Void String

type Row = [Int]

parser :: Parser [Row]
parser = (L.signed space L.decimal `sepEndBy` " ") `sepEndBy` eol

getDiffs :: Row -> Row
getDiffs (a : b : xs) = (b - a) : getDiffs (b : xs)
getDiffs _ = []

allDiffs :: Row -> [Row]
allDiffs r
  | all (== 0) r = [r]
  | otherwise = r : allDiffs (getDiffs r)

solve1 = sum . map (sum . map last . allDiffs)

solve2 = sum . map (foldr ((-) . head) 0 . allDiffs)

day9 :: Difficulty -> Problem [Row] Int
day9 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . solve1 . filter (not . null)
        Hard -> Just . solve2 . filter (not . null),
      printOutput = show
    }
