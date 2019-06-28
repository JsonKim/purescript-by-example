module Exercises where

import Prelude hiding (add)

import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref as STRef
import Data.Array (head, tail, nub, sort)
import Data.Int (toNumber)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe)
import Effect (Effect, forE)
import Effect.Exception (error, throwException)
import Effect.Random (random)
import Effect.Ref as Ref
import Math (pow)

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

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM p (x : xs) = do
  b <- p x
  xs' <- filterM p xs
  pure if b then x : xs' else xs'

simulate :: Number -> Number -> Int -> Number
simulate x0 v0 time = run do
  ref <- STRef.new { x: x0, v: v0 }
  for 0 (time * 1000) \_ -> do
    _ <- STRef.modify (\o ->  
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001  
      }) ref
    pure unit  
  final <- STRef.read ref
  pure final.x

safeDivide :: Int -> Int -> Effect Int
safeDivide _ 0 = throwException $ error "do not divide by 0"
safeDivide x y = pure (x / y)

data Point = Point Number Number

estimatePi :: Int -> Effect Number
estimatePi n = do
  pointsInCircle <- Ref.new 0
  forE 0 n $ \_ -> do
    point <- createPoint
    Ref.modify_ (\x -> x + if isInCircle point then 1 else 0) pointsInCircle

  final <- Ref.read pointsInCircle
  pure ((4.0 * (toNumber final)) / (toNumber n))

  where
    createPoint :: Effect Point
    createPoint = do
      x <- random
      y <- random
      pure (Point x y)

    isInCircle :: Point -> Boolean
    isInCircle (Point x y) = ((x - 0.5) `pow` 2.0) + ((y - 0.5) `pow` 2.0) < (0.5 `pow` 2.0)
