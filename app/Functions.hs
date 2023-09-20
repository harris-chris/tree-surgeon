module Functions
  (
    resolveFunc
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

basenameFunc :: (Show a, Eq a) => [Lit a] -> FData -> Either TSException (Lit a)
basenameFunc [(LFile a)] fData = Right $ LString a $ BS.pack $ basename fData
basenameFunc args@(x:y:z) _ = Left $ FuncWrongNumArgs "basename" (length args) 1

resolveFunc :: (Show a, Eq a) => FData -> String -> [Lit a] -> Either TSException (Lit a)
resolveFunc fData name args
    | name == "basename" = basenameFunc args fData
    | name == "==" = eqsFunc args fData
    | True = Left $ FuncNameNotRecognized name $ show <$> args

eqsFunc :: (Show a, Eq a) => [Lit a] -> FData -> Either TSException (Lit a)
eqsFunc ((LBool a x):(LBool _ y):[]) _ = Right $ LBool a $ x == y
eqsFunc args@(x:y:z) _ = Left $ FuncWrongNumArgs "==" (length args) 2

