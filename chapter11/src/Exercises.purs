module Exercises where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (Reader, ask, lift, local, runReader)
import Control.Monad.State (State, StateT, evalState, execState, get, modify, put, runState)
import Control.Monad.Writer (Writer, tell)
import Data.Array (replicate)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (even, odd)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.String (drop, joinWith, take)
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

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
            then gcd (n - m) m
            else gcd n (m - n)

gcdLog :: Int -> Int -> Writer (Array String) Int
gcdLog n 0 = pure n
gcdLog 0 m = pure m
gcdLog n m = do
  tell ["gcdLog " <> show n <> " " <> show m]
  if n > m
    then gcdLog (n - m) m
    else gcdLog n (m - n)

sumArray' :: Array Int -> Writer (Additive Int) Unit
sumArray' = traverse_ $ \n -> tell (Additive n)

collatz :: Int -> Writer (Array Int) Unit
collatz 1 = tell [1]
collatz n | even n = do
  tell [n]
  collatz (n / 2)
collatz n | odd n = do
  tell [n]
  collatz (n * 3 + 1)
collatz _ = tell [0]

split :: StateT String (Either String) String
split = do
  s <- get
  case s of
    "" -> lift $ Left "Empty string"
    _  -> do
      put  (drop 1 s)
      pure (take 1 s)

writerAndExceptT :: ExceptT String (Writer (Array String)) String
writerAndExceptT = do
  lift $ tell ["Before the error"] 
  _ <- throwError "Error!"
  lift $ tell ["After the error"] 
  pure "Return value"
