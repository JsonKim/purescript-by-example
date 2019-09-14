module Network.HTTP.Client where

import Prelude

import Control.Monad.Cont (ContT(..))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Files (FilePath, writeFileContEx)
import Types (Async)

type URI = String

foreign import getImpl ::
  Fn3 URI
      (String -> Effect Unit)
      (String -> Effect Unit)
      (Effect Unit)

get :: URI -> Async (Either String String)
get req = ContT \k -> runFn3 getImpl req (k <<< Right) (k <<< Left)

getEx :: URI -> ExceptT String Async String
getEx req = ExceptT $ get req

save :: FilePath -> String -> Async (Either String Unit)
save req path = runExceptT do
  content <- getEx req
  writeFileContEx path content
