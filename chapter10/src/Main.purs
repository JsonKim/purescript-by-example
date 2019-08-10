module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Alert (alert)
import Effect.Console (log)
import Effect.Storage (getItem)
import Foreign (ForeignError, readNullOrUndefined, readString, renderForeignError)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String
  , street     :: String
  , city       :: String
  , state      :: String
  , homePhone  :: String
  , cellPhone  :: String
  }

derive instance genericFormData :: Generic FormData _

instance decodeFormData :: Decode FormData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeFormData :: Encode FormData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

loadSavedData :: Effect (Maybe FormData) 
loadSavedData = do
  item <- getItem "person"

  let
    savedData :: Either (NonEmptyList ForeignError) (Maybe FormData)
    savedData = runExcept do
      jsonOrNull <- traverse readString =<< readNullOrUndefined item
      traverse decodeJSON jsonOrNull

  case savedData of
    Left err -> do
      alert $ "Unable to read saved form data: " <> foldMap (("\n" <> _) <<< renderForeignError) err
      pure Nothing
    Right mdata -> pure mdata

main :: Effect Unit
main = do
  log "🍝"
