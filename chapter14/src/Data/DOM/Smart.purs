module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , Measure
  , class IsValue
  , toValue
  
  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height

  , attribute, (:=)
  , text
  , elem

  , render
  , test
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

newtype Element = Element
  { name    :: String
  , attribs :: Array Attribute
  , content :: Maybe (Array Content)
  }

data Content
  = TextContent String
  | ElementContent Element

newtype Attribute = Attribute
  { key   :: String
  , value :: String
  }

newtype AttributeKey a = AttributeKey String

data Measure
  = Pixel Int
  | Percentage Int

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

instance measureIsValue :: IsValue Measure where
  toValue (Pixel m) = show m <> "px"
  toValue (Percentage m) = show m <> "%"

element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element { name, attribs, content }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute { key, value: toValue value }

infix 4 attribute as :=

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs $ Just content

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs $ Just content

img :: Array Attribute-> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey String
href = AttributeKey "href"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Measure
width = AttributeKey "width"

height :: AttributeKey Measure
height = AttributeKey "height"

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""

    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'

test :: String
test = render $ img
        [ src := "cat.jpg"
        , width := Pixel 100
        , height := Percentage 50
        ]
