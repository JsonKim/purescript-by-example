module Exercises where

import Prelude

import Data.Array ((..), null, filter)
import Data.Array.Partial (head, tail)
import Data.Foldable (product, foldl, foldr)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

isEven :: Int -> Boolean
isEven n =
  if n == 0
    then true
    else not isEven (n - 1)

squares :: Array Int -> Array Int
squares = map (\n -> n * n)

removeNagatives :: Array Int -> Array Int
removeNagatives = filter (\n -> n > 0)

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors' n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct x y = do
  xs <- x
  ys <- y
  pure [xs, ys]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ c * c == a * a + b * b
  pure [a, b, c]

allTrue :: Array Boolean -> Boolean
allTrue = foldr (==) true

-- xs = [false]
-- (foldl (==) false xs) === true

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
               then count p (unsafePartial tail xs) + 1
               else count p (unsafePartial tail xs)

count' :: forall a. (a -> Boolean) -> Array a -> Int
count' = count'' 0
  where
    count'' acc _ [] = acc
    count'' acc p xs = count'' (acc + if p (unsafePartial head xs) then 1 else 0) p (unsafePartial tail xs)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []
