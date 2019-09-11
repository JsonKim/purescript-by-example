module Doc where

import Prelude

import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (replicate)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Data.Tuple (fst)

type Level = Int
type DocA a = ReaderT Level (WriterT (Array a) Identity) a
type Doc = DocA String

line :: String -> Doc
line string = do
  level <- ask

  pure $ (indentation level) <> string
    where
      indentation :: Int -> String
      indentation x = joinWith "" (replicate x "  ")

indent :: Doc -> Doc
indent = local ((+) 1)

cat :: Array Doc -> Doc
cat x = joinWith "\n" <$> sequence x

render :: Doc -> String
render doc = fst $ unwrap $ runWriterT $ runReaderT doc 0

testDoc :: Array (DocA String)
testDoc = [ line "Here is some indented text:"
          , indent $ cat
            [ line "I am indented"
            , line "So am I"
            , indent $ line "I am even more indented"
            ]
          ]
