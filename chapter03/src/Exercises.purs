module Exercises where

import Prelude

import Data.Array ((..), null, filter)
import Data.Array.Partial (tail)
import Data.Foldable (product)
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
