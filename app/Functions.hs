module Functions
  (
    basenameFunc
    eqsFunc
    ResolvedExp (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath

-- deName then deFunc gets us here
-- data ResolvedExp =
--     RAnd ResolvedExp ResolvedExp
--     | RNot ResolvedExp
--     | ROr ResolvedExp ResolvedExp
--     -- Literals
--     | RList [ResolvedExp]
--     | RString ByteString
--     | RFile FsObjData
--     | RBool Bool
--     -- Syntax
--     | RPar ResolvedExp
--     deriving (Foldable, Functor, Eq, Show, Traversable)

fileVarName :: String
fileVarName = "file"

basenameFunc :: (Show a, Eq a) => [Exp a] -> Either TSException Exp a
basenameFunc (EString a str):[] = case str of
    "file" -> Right $ basename fsObjData
    _ -> Left FuncArgs "basename" [(Estring a str)]
basenameFunc args@(x:y:z) = Left $ FuncWrongArgsNum "basename" (length args) 2

eqsFunc :: (Show a, Eq a) => [Exp a] -> Either TSException Exp a
eqsFunc (x:y:[]) = Right $ x == y
eqsFunc args@(x:y:z) = Left $ FuncWrongArgsNum "basename" (length args) 2
