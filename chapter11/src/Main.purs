module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (RWSResult(..), runRWST)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.GameEnvironment (GameEnvironment, gameEnvironment)
import Data.GameState (GameState, initialGameState)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String (split)
import Effect (Effect)
import Effect.Console (log)
import Game (game)
import Node.ReadLine (createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)
import Node.Yargs.Applicative (Y, flag, runY, yarg)
import Node.Yargs.Setup (usage)

runGame :: GameEnvironment -> Effect Unit
runGame env = do
  interface <- createConsoleInterface noCompletion
  setPrompt "> " 2 interface

  let
    lineHandler :: GameState -> String -> Effect Unit
    lineHandler currentState input = do
      case unwrap $ runExceptT $ runRWST (game (split (wrap " ") input)) env currentState of
        Left logs -> for_ (map ((<>) "[error] ") logs) log
        Right (RWSResult state _ written) -> do
          for_ written log
          setLineHandler interface $ lineHandler state
      prompt interface

  setLineHandler interface $ lineHandler initialGameState
  prompt interface

main :: Effect Unit
main = runY (usage "$0 -p <player name>") $ map runGame env
  where
    env :: Y GameEnvironment
    env = gameEnvironment <$> yarg "p" ["player"]
                                       (Just "Player Name")
                                       (Right "The player name is required")
                                       false
                          <*> flag "d" ["debug"]
                                       (Just "Use debug mode")
