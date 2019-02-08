module Exercises where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Maybe (Maybe(..))

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

maximumOfArray :: Partial => Array Int -> Int
maximumOfArray xs = case maximum xs of
  Just max -> max

newtype Multiply = Multiply Int

instance showMultiply :: Show Multiply where
  show (Multiply x) = "Multiply " <> show x

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

-- Action 클래스의 경우 act 함수가 모든 타입을 인자로 사용하기 때문에 
-- type 추론시 모호성이 발생하지 않음. 따라서 functional dependencies가
-- 필요하지 않음
class Monoid m <= Action m a where
  act :: m -> a -> a

instance repeatAction :: Action Multiply String where
  act (Multiply n) s = act' (Multiply n) s s where
    act' (Multiply 1) s' acc = acc
    act' (Multiply n') s' acc = act' (Multiply (n' - 1)) s' (acc <> s')

instance arrayAction :: Action m a => Action m (Array a) where
  act m xs = map (act m) xs

newtype Self m = Self m

instance showSelf :: (Show a) => Show (Self a) where
  show (Self x) = "Self " <> show x

instance selfAction :: Monoid m => Action m (Self m) where
  act m (Self s) = Self (m <> s)
