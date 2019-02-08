module Stream where

import Prelude

import Data.Array as Array
import Data.String.CodeUnits as String
import Data.Maybe (Maybe(..))

class Stream stream element | stream -> element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String Char where
  uncons = String.uncons

foldStream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
    Nothing -> mempty
    Just cons -> f cons.head <> foldStream f cons.tail

genericTail :: forall l e. Stream l e => l -> Maybe l
genericTail xs = map _.tail (uncons xs)
