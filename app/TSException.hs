module TSException
  (
    ExpException(..)
    , RuntimeException(..)
  ) where

import Control.Exception
import Data.List (intercalate)

data ExpException =
    Couldn'tLex String
    | Couldn'tParse String
    | DuplicateName String String
    | FuncArgs String [String]
    | NotAFunction String [String]
    | UnrecognizedName [String] String
    deriving (Eq)

instance Exception ExpException

instance Show ExpException where
    show (Couldn'tLex expStr) =
        "Error in expression:\n" <> expStr
    show (Couldn'tParse expStr) =
        "Error in expression:\n" <> expStr
    show (DuplicateName dec name) =
        "Error: name " <> name
        <> " in declaration " <> dec
        <> " already exists"
    show (NotAFunction name args) =
        "Error: name " <> name
        <> " is being applied to arguments "
        <> (intercalate " " args)
        <> " but is not a function"
    show (UnrecognizedName varsInScope name) =
        "Error: Unrecognized name " <> name
        <> "; have names ["
        <> (intercalate ", " varsInScope)
        <> "] in scope"

data RuntimeException =
    Can'tResolveAsBool String
    | FuncWrongNumArgs String Int Int
    | FunctionNameNotRecognized String [String]
    deriving (Eq)

instance Exception RuntimeException

instance Show RuntimeException where
    show (Can'tResolveAsBool str) =
        "Error: can't resolve <> " <> str
        <> " to Bool"
    show (FuncWrongNumArgs funcName actualNum expectedNum) =
        "Error:\n" <>
        "Function " <> funcName <>
        " expects " <> show expectedNum <>
        " arguments, but has been called with " <> show actualNum
    show (FunctionNameNotRecognized name args) =
        "Error: name " <> name
        <> " with arguments "
        <> (intercalate " " args)
        <> " is not a recognized function"
