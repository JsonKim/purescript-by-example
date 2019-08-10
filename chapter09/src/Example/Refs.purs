module Example.Refs where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, fillPath, getCanvasElementById, getContext2D, rect, rotate, scale, setFillStyle, translate, withContext)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (querySelector)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

render :: Int → Context2D → Effect Unit
render count ctx = do
  setFillStyle ctx "#FFFFFF"
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 600.0
    , height: 600.0
    }

  setFillStyle ctx "#00FF00"

  withContext ctx do
    let scaleX = Math.sin (toNumber count * Math.pi / 4.0) + 1.5
    let scaleY = Math.sin (toNumber count * Math.pi / 6.0) + 1.5

    translate ctx { translateX: 300.0, translateY: 300.0 }
    rotate ctx (toNumber count * Math.pi / 18.0)
    scale ctx { scaleX, scaleY }
    translate ctx { translateX: -100.0, translateY: -100.0 }

    fillPath ctx $ rect ctx
      { x: 0.0
      , y: 0.0
      , width: 200.0
      , height: 200.0
      }

main :: Effect Unit
main = unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  clickCount <- Ref.new 0

  render 0 ctx

  doc <- toParentNode <$> (window >>= document)
  node <- querySelector (wrap "#canvas") doc

  l <- eventListener (\_ -> do
    log "Mouse Clicked!"
    Ref.modify_ (\count -> count + 1) clickCount
    count <- Ref.read clickCount
    render count ctx
  )

  for_ node $ \ele -> addEventListener (wrap "click") l false (toEventTarget ele)

  pure unit
