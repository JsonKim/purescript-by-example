module Exercises where

import Prelude

import Data.Array (head, nub, sort, tail)
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
-- 연산자를 flip하면
-- g =<< (f =<< pure a)
-- g =<< f a
-- 이는 곧 모나드 함수들간의 합성을 의미한다.

-- 여기서 결국 kleisli compostion이 유도된다.
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
