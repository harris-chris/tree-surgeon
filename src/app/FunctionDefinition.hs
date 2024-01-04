module FunctionDefinition
  (
    FunctionDefinition(..)
  ) where

import qualified Data.Text as T

import Exceptions
import FData
import IsTrace

data FunctionDefinition typeDef exprDef trc =
    FunctionDefinition {
        funcName :: T.Text
        , expectedTypes :: [typeDef]
        , funcApply :: IsTrace trc => FData -> [exprDef trc] -> EitherF trc (exprDef trc)
    }

instance Eq (FunctionDefinition a b c) where
    x == y = funcName x == funcName y

instance Show (FunctionDefinition a b c) where
    show f = T.unpack $ funcName f

