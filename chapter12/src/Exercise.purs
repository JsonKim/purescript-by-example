module Exercise where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Monad.Cont (ContT(..), runContT)
import Control.Parallel (parallel, sequential)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Files (readFileCont)
import Network.HTTP.Client (URI, get)
import Types (Async)

type Milliseconds = Int

foreign import setTimeoutImpl
  :: Fn2 Milliseconds
         (Effect Unit)
         (Effect Unit)
 
setTimeout :: Milliseconds -> (Unit -> Effect Unit) -> Effect Unit
setTimeout ms k = runFn2 setTimeoutImpl ms (k unit)

setTimeoutCont :: Milliseconds -> Async Unit
setTimeoutCont ms = ContT $ setTimeout ms

example :: Effect Unit
example = flip runContT pure do
  setTimeoutCont 1000
  log "finish1"
  setTimeoutCont 1000
  log "finish2"

combineRequest :: URI -> URI -> Effect Unit
combineRequest uri1 uri2 = flip runContT logShow do
  sequential $
    append
      <$> parallel (get uri1)
      <*> parallel (get uri2)

timeout :: forall a. Milliseconds -> Async a -> Async (Maybe a)
timeout ms fn = unwrap $
    parallel (setTimeoutCont ms >>= \_ -> pure Nothing) <|>
    parallel (fn >>= pure <<< Just)
