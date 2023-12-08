{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Common
import Data.Bifunctor
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Show, Ord)

data HandType = HighCard | Pair | TwoPair | Triplet | Fullhouse | Quartet | Quintet deriving (Ord, Eq, Show)

data Hand = Hand {cards :: [Card], bid :: Int} deriving (Eq, Show)

parseCard :: Parser Card
parseCard =
  choice
    [ Two <$ "2",
      Three <$ "3",
      Four <$ "4",
      Five <$ "5",
      Six <$ "6",
      Seven <$ "7",
      Eight <$ "8",
      Nine <$ "9",
      Ten <$ "T",
      Jack <$ "J",
      Queen <$ "Q",
      King <$ "K",
      Ace <$ "A"
    ]

parseHand :: Parser Hand
parseHand = Hand <$> some parseCard <* space <*> L.decimal

parser :: Parser [Hand]
parser = parseHand `sepEndBy` eol

countCards :: [Card] -> [(Int, Card)]
countCards = addJokers . reverse . sort . cc []
  where
    h = head . filter (\(i, c) -> c /= Joker)
    j = sum . map fst . filter (\(i, c) -> c == Joker)
    addJokers x = if length x > 1 then first (j x +) (h x) : delete (h x) (filter (\(_, c) -> c /= Joker) x) else x
    cc buff [] = buff
    cc buff (x : xc) = cc ((length $ filter (x ==) (x : xc), x) : buff) (filter (x /=) xc)

hasFive :: [Card] -> Bool
hasFive = any (\(i, c) -> i == 5) . countCards

hasFour :: [Card] -> Bool
hasFour = any (\(i, c) -> i == 4) . countCards

hasThree :: [Card] -> Bool
hasThree = any (\(i, c) -> i == 3) . countCards

hasPair :: [Card] -> Bool
hasPair = any (\(i, c) -> i == 2) . countCards

hasTwoPairs :: [Card] -> Bool
hasTwoPairs = (2 ==) . length . filter (\(i, c) -> i == 2) . countCards

hasFullhouse :: [Card] -> Bool
hasFullhouse = (&&) <$> hasThree <*> hasPair

handType :: Hand -> HandType
handType h
  | hasFive $ cards h = Quintet
  | hasFour $ cards h = Quartet
  | hasFullhouse $ cards h = Fullhouse
  | hasThree $ cards h = Triplet
  | hasTwoPairs $ cards h = TwoPair
  | hasPair $ cards h = Pair
  | otherwise = HighCard

instance Ord Hand where
  (<=) a b = if typeA /= typeB then typeA <= typeB else cards a <= cards b
    where
      typeA = handType a
      typeB = handType b

solve1 = fst . foldl (\(s, n) h -> (s + n * bid h, n + 1)) (0, 1) . sort

jackToJoker :: [Hand] -> [Hand]
jackToJoker = map (\h -> h {cards = map (\c -> if c == Jack then Joker else c) (cards h)})

solve2 = fst . foldl (\(s, n) h -> (s + n * bid h, n + 1)) (0, 1) . sort . jackToJoker

day7 :: Difficulty -> Problem [Hand] Int
day7 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . solve1
        Hard -> Just . solve2,
      printOutput = show
    }
