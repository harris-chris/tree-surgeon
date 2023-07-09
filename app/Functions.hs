module Functions
  (
    parseLitFunc
    , parseExpFunc
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath

import AST
import TSException

-- deName then deFunc gets us here
-- data ResolvedExp =
--     RAnd ResolvedExp ResolvedExp
--     | RNot ResolvedExp
--     | ROr ResolvedExp ResolvedExp
--     -- Literals
--     | RList [ResolvedExp]
--     | RString ByteString
--     | RFile FData
--     | RBool Bool
--     -- Syntax
--     | RPar ResolvedExp
--     deriving (Foldable, Functor, Eq, Show, Traversable)

fileVarName :: String
fileVarName = "file"

basenameFunc :: (Show a, Eq a) => [Lit a] -> Either TSException (FData -> Exp a)
basenameFunc [(EString a str)] = case str of
    "file" -> Right $ basename
    _ -> Left $ FuncArgs "basename" [(show $ EString a str)]
basenameFunc args@(x:y:z) = Left $ FuncWrongNumArgs "basename" (length args) 2

parseLitFunc :: (Show a, Eq a) => String -> [Lit a] -> Either TSException (FData -> Lit a)
parseLitFunc name args
    | name = "basename" = basenameFunc args

eqsFunc :: (Show a, Eq a) => [Exp a] -> Either TSException (FData -> Bool)
eqsFunc (x:y:[]) = Right $ \_ -> x == y
eqsFunc args@(x:y:z) = Left $ FuncWrongNumArgs "basename" (length args) 2
