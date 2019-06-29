module Exercises where

import Prelude

import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref as Ref
import Data.Array (head, length, nub, sort, tail)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe)

foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
foldM _ a Nil = pure a
foldM f a (b : bs) = do
  a' <- f a b
  foldM f a' bs

third :: forall a. Array a -> Maybe a
third arr = do
  r1 <- tail arr
  r2 <- tail r1
  head r2

add' :: Int -> Int -> Array Int
add' x y = [x, x+y]

sums :: Array Int -> Array Int
sums xs = sort <<< nub $ foldM add' 0 (fromFoldable xs)

filterM' :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM' _ Nil = pure Nil
filterM' p (x:xs) =
  p x >>= (\z ->
  filterM' p xs >>= (\xs' ->
  pure (if z then x : xs' else xs')))
{-
filterM p (x:xs) = do
  z <- p x
  xs' <- filterM p xs
  pure (if z then x : xs' else xs')
-}

-- pure a    >>= f    = f a                     -> left identity
-- * 여기서 f 는 (a -> m b) 타입이다.
-- * f 의 타입이 (a -> b) 라면 아래와 같이 된다
-- pure a    >>= (\a -> pure (f a)) = pure (f a)
-- * 따라서 bind가 있다면 map은 아래와 같이 쓸 수 있다.
-- map f a = a >>= (\x -> pure (f x))
-- map f a = do
--   x <- a
--   pure (f x)

-- * 또한, apply도 아래와 같다.
-- apply m a = m >>= (\f -> a >>= (\x -> pure (f x)))
-- apply m a = m >>= (\f -> map f a)
-- apply m a = do
--   f <- m
--   map f a
-- apply m a = do
--   f <- m
--   x <- a
--   pure (f x)

-- m         >>= pure = m                       -> right identity
-- (m >>= f) >>= g    = m >>= (\x -> f x >>= g) -> associativity
-- 결국 m = pure a 일때 아래와 같이 적을 수 있다.
-- (pure a >>= f) >>= g    = pure a >>= (\x -> f x >>= g) -> associativity
-- left identity와 치환에 의해서
-- (f a >>= g) = (f a >>= g)

-- (>>=)의 타입을 생각해보자
-- (>>=) :: forall m a b. Monad m => m a -> (a -> m b) -> m b
-- 연산자를 flip하면
-- (=<<) :: forall m a b. Monad m => (a -> m b) -> m a -> m b
-- g =<< (f =<< pure a)
-- g =<< f a

-- 각 a, f, g 에 대한 타입 a :: a, f :: a -> m b, g :: b -> m c 에 대해서 함수를 작성한다면
-- (<=<) :: forall m a b c. Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- 가 되고, 이것이 kleisli composition이다.
-- (>=>) :: forall a b c m. Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- (>=>) f g a = f a >>= g
-- (>=>) f g a = do
--   x <- f a
--   g x

-- pure >=> f     = f  -> left identity
-- f    >=> pure  = f  -> right identity
-- (f >=> g) >=> h = f >=> (g >=> h) -> associativity

-- f >=> g
-- \a -> (f a >>= g)
-- pure a >>= (\x -> f x >>= g) -> 결합법칙 오른쪽
-- pure a >>= (f >=> g)
-- (f >=> g) a = (f a >>= g)

-- f :: a -> b -> c
-- lift2 f a b = f <$> a <*> b
-- lift2 f (pure a) (pure b) = pure (f a b)

-- f <$> pure a <*> pure b
-- pure a >>= \x -> pure (f x) <*> pure b
-- pure (f a) <*> pure b -> left identity 에 의해서

-- pure (f a) >>= (\f' -> (pure b) >>= (\b -> pure (f' b)))
-- pure (f a) >>= (\f' -> map f' (pure b)) 
-- map (f a) (pure b) -> left identity

-- (pure b) >>= (\b -> pure ((f a) b))
-- pure ((f a) b) -> left identity
-- pure (f a b)

-- 함수 형태로 작성
-- map f (pure a) = pure (f a)
-- ap (pure f) (pure b) = pure (f b)

-- ap (map f (pure a)) (pure b)
-- ap (pure (f a)) (pure b)
-- pure ((f a) b)

sumOfSquares :: Int
sumOfSquares = run do
  total <- Ref.new 0
  Ref.read total

simulate :: Number -> Number -> Int -> Number
simulate x0 v0 time = run do
  ref <- Ref.new { x: x0, v: v0 }
  for 0 (time * 1000) \_ -> do
    Ref.modify (\o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      }
    ) ref

  final <- Ref.read ref
  pure final.x

check1 f l1 l2 = (f l1) == (f l2)
x1 = check1 length [1,2,3] [2,3,4]

check2 :: ∀ e. Eq e ⇒ (forall c. Array c → e) → (forall a. Array a) → (forall b. Array b) → Boolean
check2 f l1 l2 = (f l1) == (f l2)
x2 = check2 length [1,2,3] ["1"]
