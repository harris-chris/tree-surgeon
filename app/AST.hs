module AST
  (
    Exp(..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

data ElementType =
    File
    | Directory
    deriving (Eq, Show)

data Element = Element
    { parents :: [String]
    , fname :: String
    , elementType :: ElementType
    } deriving (Eq, Show)

class Filters a where
    filters :: a -> Element -> Bool

data Exp a =
    IsChildOf a (Exp a)
    | Or a (Exp a) (Exp a)
    | EPar a (Exp a)
    | EString a ByteString
    deriving (Foldable, Show)

instance Filters (Exp a) where
    filters (IsChildOf _ x) elem = True
    filters (Or _ x y) elem = True
    filters (EPar _ x) elem = True
    filters (EString _ x) elem = True

