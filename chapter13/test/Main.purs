module Test.Main where

import Prelude

import Data.Array (intersect, sort, sortBy)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Effect (Effect)
import Merge (merge, mergePoly, mergeWith)
import Sorted (sorted)
import Test.QuickCheck ((<?>), quickCheck)
import Tree (Tree, anywhere, insert, member)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
    go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
    go _ = true

isSubarrayOf :: forall a. (Eq a) => Array a -> Array a -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

ints :: Array Int -> Array Int
ints = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

bools :: Array Boolean -> Array Boolean
bools = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

main :: Effect Unit
main = do
  quickCheck \xs ys -> (isSorted $ merge (sorted xs) (sorted ys))
    <?> "머지 결과가 정렬되지 않았습니다. " <> show xs <> ", " <> show ys

  quickCheck \xs ys -> xs `isSubarrayOf` merge xs ys
    <?> "xs가 머지 결과의 부분 배열이 아닙니다. " <> show xs <> ", " <> show ys

  quickCheck \xs ys ->
    let 
      result = merge (sort xs) (sort ys)
    in
      xs `isSubarrayOf` result <?> show xs <> " not a subarray of" <> show result

  quickCheck \xs -> (merge xs []) == xs
    <?> "Merging " <> show xs <> " with [] doesn't produce " <> show xs <> "."

  quickCheck \xs -> (merge [] xs) == xs
    <?> "Merging " <> show xs <> " with [] doesn't produce " <> show xs <> "."

  quickCheck \xs ys -> isSorted $ ints $ mergePoly (sort xs) (sort ys)
  quickCheck \xs ys -> ints xs `isSubarrayOf` mergePoly xs ys

  quickCheck \xs ys -> isSorted $ bools $ mergePoly (sort xs) (sort ys)
  quickCheck \xs ys -> bools xs `isSubarrayOf` mergePoly xs ys

  quickCheck \xs ys f -> isSorted $ map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck \xs ys f -> xs `isSubarrayOf` mergeWith (intToBool f) xs ys

  quickCheck \t a -> member a $ insert a $ treeOfInt t
  quickCheck \f g t ->
    anywhere (\s -> f s || g  s) t == anywhere f t || anywhere g (treeOfInt t)
