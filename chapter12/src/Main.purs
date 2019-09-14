module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cont (runContT)
import Control.Monad.Except (runExceptT)
import Control.Parallel (parallel, sequential)
import Effect (Effect)
import Effect.Console (logShow)
import Files (concatenateMany, copyFileCont, readFileCont)

-- main :: Effect Unit
-- main = runContT (copyFileCont "/tmp/3.txt" "/tmp/2.txt") logShow

-- main :: Effect Unit
-- main = runContT (runExceptT $ concatenateMany ["/tmp/1.txt", "/tmp/2.txt", "/tmp/3.txt"] "/tmp/c.txt") logShow

main :: Effect Unit
main = flip runContT logShow do
  sequential $
    append
      <$> parallel (readFileCont "/tmp/1.txt")
      <*> parallel (readFileCont "/tmp/2.txt")
