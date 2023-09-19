module Functions
  (
    parseFunc
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

basenameFunc :: (Show a, Eq a) => [Lit a] -> Either TSException (FData -> Lit a)
basenameFunc [(LString a str)] = case str of
    "file" -> Right $ (\fData -> LString a $ BS.pack $ basename fData)
    _ -> Left $ FuncArgs "basename" [(show $ LString a str)]
basenameFunc args@(x:y:z) = Left $ FuncWrongNumArgs "basename" (length args) 2

parseFunc :: (Show a, Eq a) => String -> [Lit a] -> Either TSException (FData -> Lit a)
parseFunc name args
    | name == "basename" = basenameFunc args
    | name == "==" = eqsFunc args
    | True = Left $ NotAFunction name $ show <$> args

eqsFunc :: (Show a, Eq a) => [Lit a] -> Either TSException (FData -> Bool)
eqsFunc (x:y:[]) = Right $ \_ -> x == y
eqsFunc args@(x:y:z) = Left $ FuncWrongNumArgs "basename" (length args) 2

