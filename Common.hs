{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

-- src: https://gist.github.com/jiribenes/7fefdcbebfe6451915014732c825423c

import           Debug.Trace

data Difficulty = Easy | Hard

data Problem a b = Problem {parseInput :: String -> Maybe a, solve :: a -> Maybe b, printOutput :: b -> String}

runProblem :: (Show a, Show b) => FilePath -> Problem a b -> IO ()
runProblem path p = do
  input <- readFile path

  let maybeResult = do
        a <- parseInput p $ input
        traceShowM a
        b <- solve p $ a
        traceShowM b
        pure $ printOutput p $ b

  case maybeResult of
    Just result -> putStrLn result
    Nothing     -> putStrLn "ohno, error!"

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = \case
  Left err -> Nothing
  Right xs -> Just xs
