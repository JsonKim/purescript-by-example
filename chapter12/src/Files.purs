module Files where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Types (Async)

type ErrorCode = String
type FilePath = String

foreign import readFileImpl
  :: Fn3 FilePath
         (String -> Effect Unit)
         (ErrorCode -> Effect Unit)
         (Effect Unit)

foreign import writeFileImpl
  :: Fn4 FilePath
         String
         (Effect Unit)
         (ErrorCode -> Effect Unit)
         (Effect Unit)

readFile :: FilePath -> (Either ErrorCode String -> Effect Unit) -> Effect Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: FilePath -> String-> (Either ErrorCode Unit -> Effect Unit) -> Effect Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)

readFileCont :: FilePath -> Async (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont :: FilePath -> String -> Async (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text

copyFileCont :: FilePath -> FilePath -> Async (Either ErrorCode Unit)
copyFileCont src dest = do
  e <- readFileCont src
  case e of
    Left err -> pure $ Left err
    Right content -> writeFileCont dest content

concatFilesCont :: FilePath -> FilePath -> FilePath -> Async (Either ErrorCode Unit)
concatFilesCont src1 src2 dest = do
  e1 <- readFileCont src1
  e2 <- readFileCont src2
  case Tuple e1 e2 of
    Tuple (Left err) _ -> pure $ Left err
    Tuple _ (Left err) -> pure $ Left err
    Tuple (Right c1) (Right c2) -> writeFileCont dest $ c1 <> c2
