module FileOperstions where

import Prelude

import Data.Path (Path, ls, isDirectory)
import Data.Array (concatMap, (:), filter)

allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles file = filter (not isDirectory) $ allFiles file

{--
whereIs :: String -> Maybe Path
whereIs file = do
  path <- allFiles' root
  child <- ls path
  guard $ (filename child) == file
  Just path
--}
