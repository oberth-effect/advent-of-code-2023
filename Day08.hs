{-# LANGUAGE OverloadedStrings #-}

module Day08 where

import Common
import Data.Bifunctor
import Data.Map qualified as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.ParserCombinators.Parsec.Char (alphaNum)

type Parser = Parsec Void String

type Node = (String, (String, String))

type NodeMap = M.Map String (String, String)

data Input = Input {inst :: String, nodes :: [Node]} deriving (Eq, Show)

parseNode :: Parser Node
parseNode = (,) <$> some alphaNumChar <* space <* "=" <* space <* "(" <*> ((,) <$> some alphaNumChar <* "," <* space <*> some alphaNumChar <* ")")

parser :: Parser Input
parser = Input <$> some letterChar <* eol <* eol <*> parseNode `sepEndBy` eol

instructions :: Input -> String
instructions = concat . repeat . inst

step :: NodeMap -> Char -> String -> Maybe String
step nm 'L' node = fst <$> M.lookup node nm
step nm 'R' node = snd <$> M.lookup node nm
step _ _ _ = Nothing

go :: NodeMap -> [String] -> String -> Maybe Int
go nm start dirs = g dirs 0 start
  where
    g (d : ds) i curr
      | all ((==) 'Z' . last) curr = Just i
      | otherwise = g ds (i + 1) =<< traverse (step nm d) curr

solve1 i = go (M.fromList $ nodes i) ["AAA"] (instructions i)

solve2 i = go (M.fromList $ nodes i) (filter ((==) 'A' . last) (map fst $ nodes i)) (instructions i)

day8 :: Difficulty -> Problem Input Int
day8 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> solve1
        Hard -> solve2,
      printOutput = show
    }
