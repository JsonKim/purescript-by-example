module Exercises where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, evalState, execState, modify, runState)
import Data.Array (replicate)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
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

type Level = Int
type DocA a = Reader Level a
type Doc = DocA String

line :: String -> Doc
line string = do
  level <- ask

  pure $ (indentation level) <> string
    where
      indentation :: Int -> String
      indentation x = joinWith "" (replicate x "  ")

indent :: Doc -> Doc
indent = local ((+) 1)

cat :: Array Doc -> Doc
cat x = joinWith "\n" <$> sequence x

render :: Doc -> String
render doc = runReader doc 0

testDoc :: Array (DocA String)
testDoc = [ line "Here is some indented text:"
          , indent $ cat
            [ line "I am indented"
            , line "So am I"
            , indent $ line "I am even more indented"
            ]
          ]

z :: DocA (Array String)
z = sequence testDoc
