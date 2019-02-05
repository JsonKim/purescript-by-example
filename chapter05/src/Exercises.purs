module Exercises where

import Prelude

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m = if n > m
  then gcd' (n - m) m
  else gcd' n (m - n)

gcd'' :: Int -> Int -> Int
gcd'' n 0 = n
gcd'' 0 m = m
gcd'' n m | n > m     = gcd'' (n - m) m
          | otherwise = gcd'' n (m - n)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomialCoefficients :: Int -> Int -> Int
binomialCoefficients _ 0 = 1
binomialCoefficients n r | n < r     = 0
           | n == r    = 1
           | otherwise = binomialCoefficients (n-1) (r-1) + binomialCoefficients (n-1) r

sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: x } } { address: { city: y } } = x == y

getCity :: Person -> String
getCity { address: { city: city } } = city

livesInLA :: Person -> Boolean
livesInLA p = "Los Angeles" == getCity p

sameCity' :: Person -> Person -> Boolean
sameCity' a b = (getCity a) == (getCity b)

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton d _   = d
