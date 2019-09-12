module Main where

import Prelude

import Control.Monad.Cont (runContT)
import Control.Monad.Except (runExceptT)
import Effect (Effect)
import Effect.Console (logShow)
import Files (concatenateMany, copyFileCont)

-- main :: Effect Unit
-- main = runContT (copyFileCont "/tmp/3.txt" "/tmp/2.txt") logShow

main :: Effect Unit
main = runContT (runExceptT $ concatenateMany ["/tmp/1.txt", "/tmp/2.txt", "/tmp/3.txt"] "/tmp/c.txt") logShow
