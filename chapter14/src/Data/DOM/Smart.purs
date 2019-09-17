module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , ContentF
  , AttributeKey
  , Measure
  , Href
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
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

newtype Element = Element
  { name    :: String
  , attribs :: Array Attribute
  , content :: Maybe (Content Unit)
  }

newtype Name = Name String

data ContentF a
  = TextContent String a
  | ElementContent Element a
  | CommentContent String a
  | NewName (Name -> a)

instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (CommentContent c x) = CommentContent c (f x)
  map f (NewName k) = NewName (f <<< k)

type Content = Free ContentF

type Interp = WriterT String (State Int)

newtype Attribute = Attribute
  { key   :: String
  , value :: String
  }

newtype AttributeKey a = AttributeKey String

data Measure
  = Pixel Int
  | Percentage Int

data Href
  = URLHref String
  | AnchorHref Name

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

instance measureIsValue :: IsValue Measure where
  toValue (Pixel m) = show m <> "px"
  toValue (Percentage m) = show m <> "%"

instance nameIsValue :: IsValue Name where
  toValue (Name n) = n

instance hrefIsValue :: IsValue Href where
  toValue (URLHref url) = url
  toValue (AnchorHref (Name nm)) = "#" <> nm

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

newName :: Content Name
newName = liftF $ NewName identity

infix 4 attribute as :=

a :: Array Attribute -> Content Unit -> Element
a attribs content = element "a" attribs $ Just content

p :: Array Attribute -> Content Unit -> Element
p attribs content = element "p" attribs $ Just content

img :: Array Attribute-> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey Href
href = AttributeKey "href"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Measure
width = AttributeKey "width"

height :: AttributeKey Measure
height = AttributeKey "height"

name :: AttributeKey Name
name = AttributeKey "name"

render :: Element -> String
render = \e -> evalState (execWriterT (renderElement e)) 0
  where
    renderElement :: Element -> Interp Unit
    renderElement (Element e) = do
        tell "<"
        tell e.name
        for_ e.attribs $ \x -> do
          tell " "
          renderAttribute x
        renderContent e.content
      where
        renderAttribute :: Attribute -> Interp Unit
        renderAttribute (Attribute x) = do
          tell x.key
          tell "=\""
          tell x.value
          tell "\""

        renderContent :: Maybe (Content Unit) -> Interp Unit
        renderContent Nothing = tell " />"
        renderContent (Just content) = do
            tell ">"
            runFreeM renderContentItem content
            tell "</"
            tell e.name
            tell ">"
          where
            renderContentItem :: forall a. ContentF (Content a) -> Interp (Content a)
            renderContentItem (TextContent s rest) = do
              tell s
              pure rest
            renderContentItem (ElementContent e' rest) = do
              renderElement e'
              pure rest
            renderContentItem (CommentContent c rest) = do
              tell $ "<!-- " <> c <> " -->"
              pure rest
            renderContentItem (NewName k) = do
              n <- get
              let fresh = Name $ "name" <> show n
              put $ n + 1
              pure (k fresh)

test :: String
test = render $ p [] $ do
  elem $ img [ src := "cat.jpg" ]
  text "A cat"
