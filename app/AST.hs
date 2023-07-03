{-# LANGUAGE DeriveTraversable  #-}

module AST
  (
    Exp(..)
    , NamedExpr(..)
    , VarName(..)
    , FData(..)
    , IsFilePath(..)
  ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Traversable
import Debug.Trace
import System.FilePath
import System.Directory.Tree

data FData =
    FileData {
        basename :: String
        , parents :: [ByteString]
        }
    deriving (Eq, Show, Ord)

class IsFilePath a where
    toFilePath :: a -> FilePath

instance IsFilePath FData where
    toFilePath (FileData basename pts) = joinPath $ basename:(BS.unpack <$> pts)

getNameOnly :: VarName a -> ByteString
getNameOnly (VarName _ bs) = bs

type NamedExpr a = (VarName a, Exp a)

data Exp a =
    -- Logical operators
    And a (Exp a) (Exp a)
    | Not a (Exp a)
    | Or a (Exp a) (Exp a)
    -- Function
    | EFunc a (VarName a) [Exp a]
    -- Literals
    | EList a [Exp a]
    | EString a ByteString
    -- Syntax
    | EPar a (Exp a)
    | Let a [NamedExpr a] (Exp a)
    deriving (Foldable, Functor, Eq, Show, Traversable)

data VarName a
    = VarName a ByteString
    deriving (Foldable, Functor, Eq, Show, Traversable)

