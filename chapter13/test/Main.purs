module Test.Main where

import Prelude

import Data.Array (intersect, sort)
import Data.List (List(..), fromFoldable)
import Effect (Effect)
import Merge (merge, mergePoly)
import Sorted (sorted)
import Test.QuickCheck ((<?>), quickCheck)
import Tree (Tree, insert, member)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
    go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
    go _ = true

isSubarrayOf :: forall a. (Eq a) => Array a -> Array a -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

ints :: Array Int -> Array Int
ints = identity

bools :: Array Boolean -> Array Boolean
bools = identity

treeOfInt :: Tree Number -> Tree Number
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

  quickCheck \t a -> member a $ insert a $ treeOfInt t
