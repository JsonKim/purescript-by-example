module Example.Random where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (random)
import Graphics.Canvas (arc, fillPath, getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle, strokePath)
import Math (pi)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#FF0000"
  setStrokeStyle ctx "#000000"

  for_ (1 .. 100) \_ -> do
    x <- random
    y <- random
    r <- random

    let path = arc ctx 
          { x: x * 600.0
          , y: y * 600.0
          , radius: r * 50.0
          , start: 0.0
          , end: pi * 2.0
          }

    fillPath ctx path
    strokePath ctx path

