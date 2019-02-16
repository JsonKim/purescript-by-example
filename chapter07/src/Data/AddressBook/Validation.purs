module Data.AddressBook.Validation where
  
import Prelude

import Data.AddressBook (Address(..), PhoneNumber(..), address, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid)
import Partial.Unsafe (unsafePartial)

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Filed '" <> field <> "' cannot be empty"]
nonEmpty field s  = matches field entirelyWhitespaceRegex s

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len =
  invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     =
  pure unit

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (matches "State" stateRegex o.state *> pure o.state)

matches :: String -> Regex -> String -> V Errors Unit
matches _ regex value | test regex value =
  pure unit
matches field _ _ =
  invalid ["Field '" <> field <> "' did not match the required format"]

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^[A-Z]{2}$" noFlags of
      Right r -> r

entirelyWhitespaceRegex :: Regex
entirelyWhitespaceRegex =
  unsafePartial
    case regex "^[^\\s].*[^\\s]$" noFlags of
      Right r -> r
