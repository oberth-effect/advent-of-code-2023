module Day1 where

import Common
import Data.Char ( isDigit )

getDigits :: String -> String
getDigits = filter isDigit

getNumber :: String -> Integer
getNumber [] = 0
getNumber [ch] = read [ch, ch]
getNumber (ch:s) = read [ch, last s]

replace _ _ [] = []
replace find repl s =
    if take (length find) s == find
        then repl ++ replace find repl (drop (length find) s)
        else head s : replace find repl (tail s)

replaceDigits :: String -> String
replaceDigits = replace "one" "o1e" .
  replace "two" "t2o" .
  replace "three" "t3e" .
  replace "four" "f4r" .
  replace "five" "f5e" .
  replace "six" "s6x" .
  replace "seven" "s7n" .
  replace "eight" "e8t" .
  replace "nine" "n9e"



day1 :: Difficulty -> Problem [String] Integer
day1 diff =
  Problem
   { parseInput = Just . lines,
      solve = case diff of
        Easy -> Just . sum . map (getNumber . getDigits)
        Hard -> Just . sum . map (getNumber . getDigits . replaceDigits),
      printOutput = show
   }
