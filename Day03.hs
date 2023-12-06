{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import Common
import Data.Foldable
import Data.Graph
import Data.Tuple.Extra
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

type Position = (Int, Int)

data Thing = Symbol Position Char | Number Position Int deriving (Eq, Show)

getValue :: Thing -> Int
getValue = \case
  Number _ v -> v
  _ -> 0

getPosition :: Thing -> Position
getPosition (Symbol p _) = p
getPosition (Number p _) = p

isNumber = \case
  (Number _ _) -> True
  _ -> False

isSymbol = \case
  (Symbol _ _) -> True
  _ -> False

isStar = \case
  (Symbol _ '*') -> True
  _ -> False

getParserPosition = (unPos . sourceLine &&& unPos . sourceColumn) <$> getSourcePos

parseSymbol :: Parser Thing
parseSymbol =
  choice
    [ Number <$> getParserPosition <*> L.decimal,
      Symbol <$> getParserPosition <*> printChar
    ]

parseLine :: Parser [Thing]
parseLine = many (skipMany "." *> parseSymbol <* skipMany ".")

parser = parseLine `sepEndBy` eol

numLength :: Int -> Int
numLength = (+) 1 . floor . logBase 10 . fromIntegral

neigh :: Thing -> [Position]
neigh (Number (r, c) v) =
  [ (x, y)
    | x <- [r - 1 .. r + 1],
      x >= 1,
      y <- [c - 1 .. c + numLength v],
      y >= 1,
      y < c || y >= c + numLength v || x /= r
  ]
neigh (Symbol (r, c) _) =
  [ (x, y)
    | x <- [r - 1 .. r + 1],
      x >= 1,
      y <- [c - 1 .. c + 1],
      y >= 1,
      x /= r || y /= c
  ]

thingGraphList :: [Thing] -> [(Thing, Position, [Position])]
thingGraphList = map (\t -> (t, getPosition t, neigh t))

thingGraph :: [[Thing]] -> (Graph, Vertex -> (Thing, Position, [Position]), Position -> Maybe Vertex)
thingGraph = graphFromEdges . thingGraphList . concat

getComponentThings :: [[Thing]] -> [[Thing]]
getComponentThings things = map (map (fst3 . nfv) . toList) (components g)
  where
    (g, nfv, _) = thingGraph things

solve1 :: [[Thing]] -> Int
solve1 = sum . map getValue . concat . filter ((&&) <$> any isNumber <*> any isSymbol) . getComponentThings

solve2 :: [[Thing]] -> Int
solve2 = sum . map (product . map getValue) . filter ((==) 2 . length) . map (filter isNumber) . filter (any isStar) . getComponentThings

day3 :: Difficulty -> Problem [[Thing]] Int
day3 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . solve1
        Hard -> Just . solve2,
      printOutput = show
    }
