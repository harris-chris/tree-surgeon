module Functions
  (
    nameF
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath

funcName :: (Show a, Eq a) => [ResolvedExp] -> Either TSException ResolvedExp
funcName [(File a fsObjData)] = Right $ basename fsObjData
funcName args@(x:y:z) = Left $ FuncWrongArgsNum "basename" (length args) 2

