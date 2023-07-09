module TSException
  (
    TSException(..)
  ) where

import Control.Exception
import Data.List (intercalate)

data TSException =
    Couldn'tLex String
    | Couldn'tParse String
    | DuplicateName String String
    | FuncArgs String [String]
    | FuncWrongNumArgs String Int Int
    | NotAFunction String [String]
    | UnrecognizedName [String] String
    | Can'tResolveAsBool String
    deriving (Eq)

instance Exception TSException

instance Show TSException where
    show (Couldn'tLex expStr) =
        "Error in expression:\n" <> expStr
    show (Couldn'tParse expStr) =
        "Error in expression:\n" <> expStr
    show (FuncWrongNumArgs funcName actualNum expectedNum) =
        "Error:\n" <>
        "Function " <> funcName <>
        " expects " <> show expectedNum <>
        " arguments, but has been called with " <> show actualNum
    show (UnrecognizedName varsInScope name) =
        "Error: Unrecognized name " <> name
        <> "; have names ["
        <> (intercalate ", " varsInScope)
        <> "] in scope"
    show (DuplicateName dec name) =
        "Error: name " <> name
        <> " in declaration " <> dec
        <> " already exists"
    show (NotAFunction name args) =
        "Error: name " <> name
        <> " is being applied to arguments "
        <> (intercalate " " args)
        <> " but is not a function"
    show (Can'tResolveAsBool str) =
        "Error: can't resolve <> " <> str
        <> " to Bool"

