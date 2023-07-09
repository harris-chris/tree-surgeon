{-# LANGUAGE DeriveTraversable  #-}

module AST
  (
    Exp(..)
    , Lit(..)
    , NamedExp(..)
    , VarName(..)
    , FData(..)
    , IsFilePath(..)
    , getNameOnly
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

type NamedExp a = (VarName a, Exp a)

-- Everything here resolves to Bool
data Exp a =
    -- Logical operators
    And a (Exp a) (Exp a)
    | Not a (Exp a)
    | Or a (Exp a) (Exp a)
    -- Resolves to Bool
    | ELit a (Lit a) -- later we will check that Lit a is an LBool
    | EFunc a (VarName a) [Lit a]
    -- Syntax
    | EPar a (Exp a)
    | ELet a [NamedExp a] (Exp a)
    deriving (Foldable, Functor, Eq, Show, Traversable)

-- Everything here resolves to a Literal, used as arguments to EFunc
data Lit a =
    -- Literals
    LBool a Bool
    | LList a [Lit a]
    | LString a ByteString
    -- Function, resolves to Lit
    | LFunc a (VarName a) [Lit a]
    -- Syntax
    | LPar a (Lit a)
    | LLet a [NamedExp a] (Lit a)
    deriving (Foldable, Functor, Eq, Show, Traversable)

data VarName a
    = VarName a ByteString
    deriving (Foldable, Functor, Eq, Show, Traversable)

