module Main where

import Prelude

import Data.Either (Either(..))
import Data.GameEnvironment (GameEnvironment, gameEnvironment)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.ReadLine (close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)
import Node.Yargs.Applicative (Y, flag, runY, yarg)
import Node.Yargs.Setup (usage)

runGame :: GameEnvironment -> Effect Unit
runGame env = do
  interface <- createConsoleInterface noCompletion
  setPrompt "> " 2 interface

  let
    lineHandler :: String -> Effect Unit
    lineHandler input =
      if input == "quit"
        then close interface
        else do
          log $ "You typed: " <> input
          prompt interface

  setLineHandler interface lineHandler
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
