{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import Common
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

data Card = Card {cid :: Int, winning :: [Int], scratched :: [Int], copies :: Int} deriving (Eq, Show)

parseNumbers :: Parser [Int]
parseNumbers = (space *> L.decimal) `sepEndBy` " "

parseCard :: Parser Card
parseCard = Card <$> ("Card" *> space *> L.decimal <* ":") <*> parseNumbers <* "|" <*> parseNumbers <*> pure 1

parser :: Parser [Card]
parser = parseCard `sepEndBy` eol

solve1 :: [Card] -> Int
solve1 = sum . map (\i -> 2 ^ (i - 1)) . filter (0 <) . map (length . (intersect <$> scratched <*> winning))

addCopies :: Int -> [Card] -> [Card]
addCopies num = map (\g -> g {copies = copies g + num})

play2 :: [Card] -> [Card] -> [Card]
play2 buff [] = buff
play2 buff (x : xs) = play2 (x : buff) newXs
  where
    win = length $ (intersect <$> scratched <*> winning) x
    newXs = addCopies (copies x) (take win xs) ++ drop win xs

solve2 :: [Card] -> Int
solve2 = sum . map copies . play2 []

day4 :: Difficulty -> Problem [Card] Int
day4 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . solve1
        Hard -> Just . solve2,
      printOutput = show
    }
