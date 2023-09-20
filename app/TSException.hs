module TSException
  (
    TSException(..)
  ) where

import Control.Exception
import Data.List (intercalate)

data TSException =
    Can'tApplyLogicalToNonBool String
    | Can'tResolveAsBool String
    | CanOnlyFilterDirectory String
    | Couldn'tLex String
    | Couldn'tParse String
    | DuplicateName String String
    | FuncNameNotRecognized String [String]
    | FuncWrongNumArgs String Int Int
    | NotAFunction String [String]
    | UnrecognizedName [String] String
    deriving (Eq)

instance Exception TSException

instance Show TSException where
    show (Can'tApplyLogicalToNonBool str) =
        "Error: can't invert non-boolean <> " <> str
    show (Can'tResolveAsBool str) =
        "Error: can't resolve <> " <> str
        <> " to Bool"
    show (CanOnlyFilterDirectory str) =
        "Error: " <> str
        <> " is a file, but I can only filter directories"
    show (Couldn'tLex expStr) =
        "Error in expression:\n" <> expStr
    show (Couldn'tParse expStr) =
        "Error in expression:\n" <> expStr
    show (DuplicateName dec name) =
        "Error: name " <> name
        <> " in declaration " <> dec
        <> " already exists"
    show (FuncNameNotRecognized name args) =
        "Error: name " <> name
        <> " with arguments "
        <> (intercalate " " args)
        <> " is not a recognized function"
    show (FuncWrongNumArgs funcName actualNum expectedNum) =
        "Error:\n" <>
        "Function " <> funcName <>
        " expects " <> show expectedNum <>
        " arguments, but has been called with " <> show actualNum
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

