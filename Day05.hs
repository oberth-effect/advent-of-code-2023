{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Common
import Data.IntMap (findWithDefault)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

type Mapping = (Int, Int, Int)

data Input = Input {seeds :: [Int], sts :: [Mapping], stf :: [Mapping], ftw :: [Mapping], wtl :: [Mapping], ltt :: [Mapping], tth :: [Mapping], htl :: [Mapping]} deriving (Eq, Show)

parseMapping :: Parser (Int, Int, Int)
parseMapping = (,,) <$> L.decimal <*> (space *> L.decimal) <*> (space *> L.decimal)

parseMap :: Parser [Mapping]
parseMap = parseMapping `sepEndBy` eol

parser :: Parser Input
parser =
  Input
    <$> ("seeds:" *> space *> L.decimal `sepEndBy` " " <* eol)
    <*> (eol *> "seed-to-soil map:" *> eol *> parseMap)
    <*> (eol *> "soil-to-fertilizer map:" *> eol *> parseMap)
    <*> (eol *> "fertilizer-to-water map:" *> eol *> parseMap)
    <*> (eol *> "water-to-light map:" *> eol *> parseMap)
    <*> (eol *> "light-to-temperature map:" *> eol *> parseMap)
    <*> (eol *> "temperature-to-humidity map:" *> eol *> parseMap)
    <*> (eol *> "humidity-to-location map:" *> eol *> parseMap)

applyMapping :: [Mapping] -> Int -> Int
applyMapping [] i = i
applyMapping ((d, s, r) : ms) i = if i < s + r && i >= s then d + i - s else applyMapping ms i

applyMappings :: [Mapping] -> [Int] -> [Int]
applyMappings mp = map (applyMapping mp)

solve1 :: Input -> Int
solve1 i =
  minimum $
    applyMappings (htl i) $
      applyMappings (tth i) $
        applyMappings (ltt i) $
          applyMappings (wtl i) $
            applyMappings (ftw i) $
              applyMappings (stf i) $
                applyMappings (sts i) (seeds i)

type Interval = (Int, Int)

hasOverlap :: Interval -> Interval -> Bool
hasOverlap (a, ar) (b, br) = a + ar > b && b + br > a

getOverlap :: Interval -> Interval -> Maybe Interval
getOverlap (a, ar) (b, br) = if hasOverlap (a, ar) (b, br) then Just (max a b, min (a + ar) (b + br) - max a b) else Nothing

getBelow :: Interval -> Interval -> Maybe Interval
getBelow (a, ar) (b, br) = if b < a then Just (b, a - b) else Nothing

getAbove :: Interval -> Interval -> Maybe Interval
getAbove (a, ar) (b, br) = if b + br > a + ar then Just (a + ar, b + br - a - ar) else Nothing

transformSeeds :: [Interval] -> [Int] -> [Interval]
transformSeeds buff (x : r : xs) = transformSeeds ((x, r) : buff) xs
transformSeeds buff _ = buff

applyMappingInterval :: [Mapping] -> Interval -> [Interval]
applyMappingInterval [] i = [i]
applyMappingInterval ((d, s, r) : mx) i = case getOverlap (s, r) i of
  Nothing -> applyMappingInterval mx i
  Just (o, or) -> under ++ overlap ++ above
    where
      overlap = [(d + o - s, or)]
      under = case getBelow (s, r) i of
        Nothing -> []
        Just b -> applyMappingInterval mx b
      above = case getAbove (s, r) i of
        Nothing -> []
        Just a -> applyMappingInterval mx a

applyMappingIntervals :: [Mapping] -> [Interval] -> [Interval]
applyMappingIntervals m = concatMap (applyMappingInterval m)

solve2 :: Input -> Int
solve2 i =
  minimum $
    concatMap (\(a, b) -> [a, a + b - 1]) $
      applyMappingIntervals (htl i) $
        applyMappingIntervals (tth i) $
          applyMappingIntervals (ltt i) $
            applyMappingIntervals (wtl i) $
              applyMappingIntervals (ftw i) $
                applyMappingIntervals (stf i) $
                  applyMappingIntervals (sts i) (transformSeeds [] (seeds i))

day5 :: Difficulty -> Problem Input Int
day5 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . solve1
        Hard -> Just . solve2,
      printOutput = show
    }
