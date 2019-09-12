module Types where

import Prelude

import Control.Monad.Cont (ContT)
import Effect (Effect)

type Async = ContT Unit Effect
