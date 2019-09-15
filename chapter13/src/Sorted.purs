module Sorted where

import Prelude

import Data.Array (sort)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)

newtype Sorted a = Sorted (Array a)

sorted :: forall a. Sorted a -> Array a
sorted (Sorted xs) = xs

instance showSorted :: Show a => Show (Sorted a) where
  show = show <<< sorted

instance arbSorted :: (Arbitrary a, Ord a) => Arbitrary (Sorted a) where
  arbitrary = map (Sorted <<< sort) arbitrary

instance coarbSorted :: (Coarbitrary a) => Coarbitrary (Sorted a) where
  coarbitrary (Sorted arr) = coarbitrary arr

newtype Byte = Byte Int

instance arbByte :: Arbitrary Byte where
  arbitrary = map intToByte arbitrary
    where
      intToByte n | n >= 0 = Byte (n `mod` 256)
                  | otherwise = intToByte (-n)

instance coarbByte :: Coarbitrary Byte where
  coarbitrary (Byte a) = coarbitrary a
