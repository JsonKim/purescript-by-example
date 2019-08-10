module Example.LSystem where

import Prelude

import Data.Foldable (foldM)
import Data.List (List(..), concatMap, (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

data Alphabet = L | R | F

type Sentence = List Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = F : R : R : F : R : R : F : R : R : Nil

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

productions :: Alphabet -> Sentence
productions L = L : Nil
productions R = R : Nil
productions F = F : L : F : R : R : F : L : F : Nil

lsystem :: forall m a s
   . Monad m
  => List a
  -> (a -> List a)
  -> (s -> a -> m s)
  -> Int
  -> s
  -> m s
lsystem init prod interpret count state = go init count
  where
    go s 0 = foldM interpret state s
    go s n = go (concatMap prod s) (n - 1)

main' :: Effect Unit
main' = unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    interpret :: State -> Alphabet -> Effect State
    interpret state L = pure $ state { theta = state.theta - Math.pi / 3.0 }
    interpret state R = pure $ state { theta = state.theta + Math.pi / 3.0 }
    interpret state F = do
      let
        x = state.x + Math.cos state.theta * 1.5
        y = state.y + Math.sin state.theta * 1.5

      moveTo ctx state.x state.y
      lineTo ctx x y
      pure { x, y, theta: state.theta }

  setStrokeStyle ctx "#000000"
  _ <- strokePath ctx $ lsystem initial productions interpret 5 initialState
  pure unit

main :: Effect Unit
main = unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    interpret :: State -> Alphabet -> Effect State
    interpret state L = pure $ state { theta = state.theta - Math.pi / 3.0 }
    interpret state R = pure $ state { theta = state.theta + Math.pi / 3.0 }
    interpret state F = do
      let
        x = state.x + Math.cos state.theta * 1.5
        y = state.y + Math.sin state.theta * 1.5

      lineTo ctx x y
      pure { x, y, theta: state.theta }

  setFillStyle ctx "#000000"
  fillPath ctx do
    moveTo ctx initialState.x initialState.y
    _ <- lsystem initial productions interpret 5 initialState
    closePath ctx

  pure unit
