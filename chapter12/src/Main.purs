module Main where

import Prelude

import Control.Monad.Cont (runContT)
import Effect (Effect)
import Effect.Console (logShow)
import Files (copyFileCont)

main :: Effect Unit
main = runContT (copyFileCont "/tmp/3.txt" "/tmp/2.txt") logShow
