module Game where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.RWS (RWST, ask, get, modify_, put, tell)
import Data.Coords (Coords(..), coords, prettyPrintCoords)
import Data.Foldable (foldr, for_)
import Data.GameEnvironment (GameEnvironment(..))
import Data.GameItem (GameItem(..), readItem)
import Data.GameState (GameState(..))
import Data.Identity (Identity)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S

type Log = L.List String

type Game = RWST GameEnvironment Log GameState (ExceptT Log Identity)

has :: GameItem -> Game Boolean
has item = do
  GameState state <- get
  pure $ item `S.member` state.inventory

pickUp :: GameItem -> Game Unit
pickUp item = do
  GameState state <- get
  case state.player `M.lookup` state.items of
    Just items | item `S.member` items -> do
      let newItems = M.update (Just <<< S.delete item) state.player state.items
          newInventory = S.insert item state.inventory
      put $ GameState state { items = newItems
                            , inventory = newInventory
                            }
      tell (L.singleton ("You now have the " <> show item))
    _ -> tell (L.singleton "I don't see that item here.")

describeRoom :: Game Unit
describeRoom = do
  GameState state <- get
  case state.player of
    Coords { x: 0, y: 0 } -> tell (L.singleton "You are in a dark forest. You see a path to the north.")
    Coords { x: 0, y: 1 } -> tell (L.singleton "You are in a clearing.")
    _ -> tell (L.singleton "You are deep in the forest.")

move :: Int -> Int -> Game Unit
move dx dy = modify_ (\(GameState state) -> GameState (state { player = updateCoords state.player }))
  where
    updateCoords :: Coords -> Coords
    updateCoords (Coords p) = coords (p.x + dx) (p.y + dy)

use :: GameItem -> Game Unit
use Candle = tell (L.singleton "I don't know what you want me to do with that.")
use Matches = do
  hasCandle <- has Candle
  if hasCandle
    then do
      GameEnvironment env <- ask
      tell (L.fromFoldable [ "You light the candle."
                           , "Congratulations, " <> env.playerName <> "!"
                           , "You win!"
                           ])
    else tell (L.singleton "You don't have anything to light.")

game :: Array String -> Game Unit
game ["look"] = do
  GameState state <- get
  tell (L.singleton ("You are at " <> prettyPrintCoords state.player))
  describeRoom
  for_ (M.lookup state.player state.items) $ \items ->
    tell (map (\item -> "You can see the " <> show item <> ".") (S.toUnfoldable items :: L.List GameItem))
game ["inventory"] = do
  GameState state <- get
  tell (map (\item -> "You have the " <> show item <> ".") (S.toUnfoldable state.inventory :: L.List GameItem))
game ["north"] = move 0    1
game ["south"] = move 0    (-1)
game ["west"]  = move (-1) 0
game ["east"]  = move 1    0
game ["take", item] =
  case readItem item of
    Nothing -> tell (L.singleton "I don't know what item you are referring to.")
    Just gameItem -> pickUp gameItem
game ["use", item] =
  case readItem item of
    Nothing -> tell (L.singleton "I don't know what item you are referring to.")
    Just gameItem -> do
      hasItem <- has gameItem
      if hasItem
        then use gameItem
        else tell (L.singleton "You don't have that item.")
game ["cheat"] = do
  GameEnvironment env <- ask
  if env.cheatMode
    then do
      GameState state <- get
      put $ GameState state
        { items = M.empty
        , inventory = foldr S.union S.empty state.items
        }
    else throwError (L.singleton "Not running in cheat mode.")
game ["debug"] = do
  GameEnvironment env <- ask
  if env.debugMode
    then do
      state :: GameState <- get
      tell (L.singleton (show state))
    else throwError (L.singleton "Not running in debug mode.")
game [] = pure unit
game _  = tell (L.singleton "I don't understand.")
