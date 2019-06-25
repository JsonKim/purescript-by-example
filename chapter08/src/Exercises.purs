module Exercises where

import Prelude

import Data.Array (head, nub, sort, tail)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe)

foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
foldM _ a Nil = pure a
foldM f a (b : bs) = do
  a' <- f a b
  foldM f a' bs

third :: forall a. Array a -> Maybe a
third arr = do
  r1 <- tail arr
  r2 <- tail r1
  head r2

add' :: Int -> Int -> Array Int
add' x y = [x, x+y]

sums :: Array Int -> Array Int
sums xs = sort <<< nub $ foldM add' 0 (fromFoldable xs)
