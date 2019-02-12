module Exercises where

import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Data.List (List(..))

combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

optionalAdd :: forall f a. Apply f => Semiring a => f a -> f a -> f a
optionalAdd = lift2 (+)

optionalSub :: forall f a. Apply f => Ring a => f a -> f a -> f a
optionalSub = lift2 (-)

optionalMul :: forall f a. Apply f => Semiring a => f a -> f a -> f a
optionalMul = lift2 (*)

optionalDiv :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
optionalDiv = lift2 (/)

combineMaybe :: forall f a. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x
