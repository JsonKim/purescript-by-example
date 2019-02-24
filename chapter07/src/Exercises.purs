module Exercises where

import Prelude

import Control.Apply (lift2)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)

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

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: (Show a) => Show (Tree a) where
  show Leaf = "Tree (Leaf)"
  show (Branch left value right) = (show value) <> "\n" <> "  \\" <> (show left) <> "\n" <> "  \\" <> (show right)

instance functorTree :: Functor Tree where
  map f Leaf = Leaf
  map f (Branch left value right) = Branch (map f left) (f value) (map f right)

instance foldableTree :: Foldable Tree where
  foldr _ acc Leaf = acc
  foldr f acc (Branch l v r) = foldr f (f v (foldr f acc r)) l
  foldl _ acc Leaf = acc
  foldl f acc (Branch l v r) = foldl f (f (foldl f acc l) v) r
  foldMap _ Leaf = mempty
  foldMap f (Branch l v r) = foldMap f l <> f v <> foldMap f r

instance traverseTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch left value right) = Branch <$> (traverse f left) <*> f value <*> (traverse f right)
  sequence Leaf = pure Leaf
  sequence (Branch l v r) = Branch <$> sequence l <*> f v <*> sequence r

leaf :: forall a. Tree a
leaf = Leaf

tree :: forall a. a -> Tree a -> Tree a -> Tree a
tree value left right = Branch left value right

exampleTree :: Tree Int
exampleTree = tree 1
  (tree 2 
    (tree 22 leaf leaf)
    (tree 23 leaf leaf))
  (tree 3 leaf leaf)
