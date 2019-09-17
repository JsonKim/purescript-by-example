module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , ContentF
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

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

newtype Element = Element
  { name    :: String
  , attribs :: Array Attribute
  , content :: Maybe (Content Unit)
  }

data ContentF a
  = TextContent String a
  | ElementContent Element a
  | CommentContent String a

instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (CommentContent c x) = CommentContent c (f x)

type Content = Free ContentF

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

element :: String -> Array Attribute -> Maybe (Content Unit) -> Element
element name attribs content = Element { name, attribs, content }

text :: String -> Content Unit
text s = liftF $ TextContent s unit

comment :: String -> Content Unit
comment s = liftF $ CommentContent s unit

elem :: Element -> Content Unit
elem e = liftF $ ElementContent e unit

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute { key, value: toValue value }

infix 4 attribute as :=

a :: Array Attribute -> Content Unit -> Element
a attribs content = element "a" attribs $ Just content

p :: Array Attribute -> Content Unit -> Element
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
render = execWriter <<< renderElement
  where
    renderElement :: Element -> Writer String Unit
    renderElement (Element e) = do
        tell "<"
        tell e.name
        for_ e.attribs $ \x -> do
          tell " "
          renderAttribute x
        renderContent e.content
      where
        renderAttribute :: Attribute -> Writer String Unit
        renderAttribute (Attribute x) = do
          tell x.key
          tell "=\""
          tell x.value
          tell "\""

        renderContent :: Maybe (Content Unit) -> Writer String Unit
        renderContent Nothing = tell " />"
        renderContent (Just content) = do
            tell ">"
            runFreeM renderContentItem content
            tell "</"
            tell e.name
            tell ">"
          where
            renderContentItem :: forall a. ContentF (Content a) -> Writer String (Content a)
            renderContentItem (TextContent s rest) = do
              tell s
              pure rest
            renderContentItem (ElementContent e' rest) = do
              renderElement e'
              pure rest
            renderContentItem (CommentContent c rest) = do
              tell $ "<!-- " <> c <> " -->"
              pure rest

test :: String
test = render $ p [] $ do
  elem $ img [ src := "cat.jpg" ]
  text "A cat"
