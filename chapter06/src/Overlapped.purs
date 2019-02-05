module Overlapped where

import Prelude

data T = T

instance showT1 :: Show T where
  show _ = "Instance 1"

{-
instance showT2 :: Show T where
  show _ = "Instance 2"
-}
