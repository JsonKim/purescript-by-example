module Data.AddressBook.UI
  ( createCounter
  , createAddressBook
  ) where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array (length, modifyAt, zipWith, (..))
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, stopPropagation, targetValue)
import React.Basic.Events (EventHandler, handler, handler_, merge)
import React.Basic.Hooks (CreateComponent, Hook, JSX, UseState, component, fragment, useState, (/\))
import React.Basic.Hooks as React

type Counter =
  { value :: Int
  , increase :: EventHandler
  , decrease :: EventHandler
  , reset :: EventHandler
  }

useCounter :: Int -> Hook (UseState Int) Counter
useCounter initialCount = React.do
  count /\ setCounter <- useState initialCount
  pure
    { value: count
    , increase: handler_ $ setCounter (_ + 1)
    , decrease: handler_ $ setCounter (_ - 1)
    , reset: handler_ $ setCounter (\_ -> initialCount)
    }

createCounter :: CreateComponent {}
createCounter = do
  component "Counter" \props -> React.do
    counter <- useCounter 0

    pure $ fragment
      [ R.button
          { onClick: counter.reset
          , children: [ R.text "Reset" ]
          }
      , R.button
          { onClick: counter.increase
          , children: [ R.text "+" ]
          }
      , R.button
          { onClick: counter.decrease
          , children: [ R.text "-" ]
          }
       , R.div_ [ R.text $ show counter.value ]
      ]

    where
      increase setCounter = setCounter (_ + 1)

newtype AppState = AppState
  { person :: Person
  , errors :: Errors
  }

initialState :: AppState
initialState = AppState
  { person: examplePerson
  , errors: []
  }

type AppStateData =
  { person  :: Person
  , address :: Address
  , errors  :: Errors
  , setState :: (AppState -> AppState) -> Effect Unit
  }

useAppState :: AppState -> Hook (UseState AppState) AppStateData
useAppState initial = React.do
  (AppState { person: person@(Person { homeAddress: address }), errors }) /\ setState <- (useState initial)
  pure
    { person
    , address
    , errors
    , setState
    }

renderValidatiaonError :: String -> JSX
renderValidatiaonError err = R.li_ [R.text err]

renderValidatiaonErrors :: Errors -> Array JSX
renderValidatiaonErrors [] = []
renderValidatiaonErrors xs = [ R.div { className: "alert alert-danger", children: map renderValidatiaonError xs } ]

formField' :: ((AppState → AppState) → Effect Unit) → String → String → String → (String → Person) → JSX
formField' setState name hint value update =
  R.div { className: "form-group", children:
    [ R.label { className: "col-sm-2 control-label", children: [ R.text name ] }
    , R.div   { className: "col-sm-3", children: 
        [ R.input
            { type: "text"
            , className: "form-control"
            , placeholder: hint
            , value: value
            , onChange: handler (preventDefault >>> stopPropagation >>> merge { targetValue }) (updateAppState setState update)
            }]}]} 

updateAppState :: ((AppState -> AppState) -> Effect Unit) -> (String -> Person) -> { targetValue :: Maybe String } -> Effect Unit
updateAppState setState update { targetValue } = do
  let value = fromMaybe "" targetValue
  let newPerson = update value

  log "Running validators"
  case validatePerson' newPerson of
    Left errors -> setState $ const (AppState { person: newPerson, errors: errors })
    Right _ -> setState $ const (AppState { person: newPerson, errors: [] })

createAddressBook :: CreateComponent {}
createAddressBook = do
  component "AddressBook" \props -> React.do
    appState <- useAppState initialState

    let
      person  = case appState.person  of (Person  p) -> p
      address = case appState.address of (Address a) -> a

      formField = formField' appState.setState

      renderPhoneNumber (PhoneNumber phone) index =
        formField (show phone."type") "XXX-XXX-XXXX" phone.number \s ->
          Person $ person { phones = fromMaybe person.phones $ modifyAt index (updatePhoneNumber s) person.phones }
          
      updateFirstName s = Person $ person { firstName = s }
      updateLastName  s = Person $ person { lastName  = s }

      updateStreet s = Person $ person { homeAddress = Address $ address { street = s } }
      updateCity   s = Person $ person { homeAddress = Address $ address { city   = s } }
      updateState  s = Person $ person { homeAddress = Address $ address { state  = s } }

      updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }

    pure $
      R.div { className: "container" , children:
        [ R.div { className: "row", children: renderValidatiaonErrors appState.errors }
        , R.div { className: "row", children:
            [ R.form { className: "form-horizontal", children:
                [ R.h3_ [ R.text "Basic Information" ]
                , formField "First Name" "First Name" person.firstName updateFirstName
                , formField "Last Name"  "Last Name"  person.lastName  updateLastName

                , R.h3_ [ R.text "Address" ]
                , formField "Street" "Street" address.street updateStreet
                , formField "City"   "City"   address.city   updateCity
                , formField "State"  "State"  address.state  updateState

                , R.h3_ [ R.text "Contact Information" ]
                ]
                <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
                }]}]}
