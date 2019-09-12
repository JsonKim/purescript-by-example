module Exercise where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
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
example = runContT (do
  setTimeoutCont 1000
  log "finish") logShow
