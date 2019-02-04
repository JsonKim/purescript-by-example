module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Array (null, filter)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)

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

main :: Effect Unit
main = do
  log "Hello sailor!"
