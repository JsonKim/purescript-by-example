module Example.Rectangle where

import Prelude

import Data.Array (head, tail)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Graphics.Canvas (Context2D, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, rect, setFillStyle, setStrokeStyle, strokePath)
import Partial.Unsafe (unsafePartial)

type Point = { x :: Number, y :: Number }

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath _ [] = pure unit
renderPath _ [_] = pure unit
renderPath ctx points = unsafePartial $ do
  let (Just first) = head points
  let (Just remain) = tail points
  setStrokeStyle ctx "#000000"
  strokePath ctx $ do
    moveTo ctx first.x first.y
    _ <- traverse (\p -> lineTo ctx p.x p.y) remain
    closePath ctx

renderPath' :: Context2D -> List Point -> Effect Unit
renderPath' _ Nil = pure unit
renderPath' ctx (first : remain) = do
  setStrokeStyle ctx "#000000"
  strokePath ctx $ do
    moveTo ctx first.x first.y
    _ <- traverse (\p -> lineTo ctx p.x p.y) remain
    closePath ctx

main :: Effect Unit
main = unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#0000FF"
  fillPath ctx $ rect ctx
    { x: 250.0 
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  renderPath' ctx ({ x: 10.0, y: 20.0 } : { x: 30.0, y: 49.0 } : { x: 10.0, y: 60.0 } : Nil)
