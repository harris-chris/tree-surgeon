module AST
  (
    Exp(..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Exp a =
    IsChildOf a (Exp a)
    | Or a (Exp a) (Exp a)
    | EPar a (Exp a)
    | EString a ByteString
    deriving (Foldable, Show)

