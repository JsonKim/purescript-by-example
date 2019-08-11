module Exercises where

import Prelude

import Control.Monad.State (State, evalState, execState, modify, runState)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple)

modifySum :: Int -> State Int Int
modifySum n = modify \sum -> sum + n

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ modifySum

execSumArray :: Int
execSumArray = execState (do
  sumArray [1, 2, 3]
  sumArray [4, 5]
  sumArray [6]
) 0

runSumArray :: Tuple Unit Int
runSumArray = runState (do
  sumArray [1, 2, 3]
  sumArray [4, 5]
  sumArray [6]
) 0

evalSumArray :: Unit
evalSumArray = evalState (do
  sumArray [1, 2, 3]
  sumArray [4, 5]
  sumArray [6]
) 0

testParens :: String -> Boolean
testParens input = case result of
  Just 0 -> true
  _ -> false

  where
    m :: Char -> Maybe Int -> Maybe Int
    m _   Nothing  = Nothing
    m ')' (Just 0) = Nothing
    m '(' (Just n) = Just (n + 1)
    m ')' (Just n) = Just (n - 1)
    m _   (Just n) = Just n

    calc :: Char -> State (Maybe Int) (Maybe Int)
    calc x = modify (m x)

    result :: Maybe Int
    result = execState (traverse_ calc (toCharArray input)) (Just 0)
