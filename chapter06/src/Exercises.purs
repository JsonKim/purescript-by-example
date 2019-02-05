module Exercises where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr)

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real: r, imaginary: i }) =
    show r <> " + " <> show i <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) =
    r1 == r2 && i1 == i2

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty x xs) = show ([x] <> xs)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = (x == y) && (xs == ys)

instance semigroupNonEmpty :: (Semigroup a) => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f a (NonEmpty x xs) = foldr f a $ [x] <> xs
  foldl f a (NonEmpty x xs) = foldl f a $ [x] <> xs
  foldMap f (NonEmpty x xs) = foldMap f $ [x] <> xs

data Extended a = Finite a | Infinite

instance showExtended :: (Show a) => Show (Extended a) where
  show Infinite = "∞"
  show (Finite x) = "Finite " <> show x

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq Infinite Infinite = true
  eq Infinite _ = false
  eq _ Infinite = false
  eq (Finite x) (Finite y) = eq x y

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldr f a (OneMore x xs) = f x (foldr f a xs)
  foldl f a (OneMore x xs) = f (foldl f a xs) x
  foldMap f (OneMore x xs) = f x <> (foldMap f xs)
