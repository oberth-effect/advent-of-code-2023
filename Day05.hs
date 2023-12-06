{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Common
import Data.Map
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

type Smap = Map Int (Int, Int)

data Input = Input {seeds :: [Int], sts :: Smap, stf :: Smap, ftw :: Smap, wtl :: Smap, ltt :: Smap, tth :: Smap, htl ::Smap}


--parseMap :: Parser Smap


day5 :: Difficulty -> Problem Input Int
day5 diff =
  Problem
    { parseInput = eitherToMaybe . parse parser "",
      solve = case diff of
        Easy -> Just . const 0
        Hard -> Just . const 0,
      printOutput = show
    }
