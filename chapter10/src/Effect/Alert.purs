module Effect.Alert where

import Data.Unit (Unit)
import Effect (Effect)

foreign import alert :: String -> Effect Unit
