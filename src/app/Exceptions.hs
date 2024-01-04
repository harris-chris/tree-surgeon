{-# LANGUAGE OverloadedStrings #-}

module Exceptions
  (
    EitherF
    , FilterException(..)
    , OtherException(..)
    , TSException(..)
  ) where

import Control.Exception
import Data.Text as T
import Data.Typeable
import TextShow

import ResolvedType

type EitherF a result = Either (FilterException a) result

-- TODO: give this 'a' parameters for all constructors, implement IsTrace, and use that in
-- toErrorMessage
data FilterException a =
    Can'tResolveAsBool a Text ResolvedType
    | DuplicateName a Text
    | FuncArgWrongType {
        fawtOriginal :: a
        , fawtFuncName :: Text
        , fawtArgNum :: Int
        , fawtActualType :: ResolvedType
        , fawtExpectedType :: ResolvedType
    }
    | FuncNameNotRecognized a Text
    | FuncWrongNumArgs a Text Int Int
    | ListNotHeterogeneous a [Text]
    | NotAFunction a Text
    | UnrecognizedName a [Text] Text
    deriving (Eq)

instance TextShow (FilterException a) where
    showb (Can'tResolveAsBool _ expr exprType) =
        fromText $
            "Error: filter expression resolves to " <> expr
            <> " which is type " <> showt exprType
            <> " but must be of type Boolean"
    showb (DuplicateName _ name) =
        fromText $
            "Error: name " <> name
            <> " already exists"
    showb err@(FuncArgWrongType {}) =
        fromText $
            "Error: function " <> fawtFuncName err
            <> " was expecting type " <> (showt $ fawtExpectedType err)
            <> " but got type " <> (showt $ fawtActualType err)
    showb (FuncNameNotRecognized _ name) =
        fromText $
            "Error: name " <> name
            <> " is not a recognized function name"
    showb (FuncWrongNumArgs _ funcName actualNum expectedNum) =
        fromText $
            "Error:\n" <>
            "Function " <> funcName <>
            " expects " <> (pack $ show expectedNum) <>
            " arguments, but has been called with " <> (pack $ show actualNum)
    showb (UnrecognizedName _ varsInScope name) =
        fromText $
            "Error: Unrecognized name " <> name
            <> "; have names ["
            <> T.intercalate ", " varsInScope
            <> "] in scope"

instance Show (FilterException a) where
    show e = show $ showb e

instance Typeable a => Exception (FilterException a)

data OtherException =
    CanOnlyFilterDirectory Text
    | Can'tParse
    | FailedToReadTree Text

instance TextShow OtherException where
    showb (CanOnlyFilterDirectory fileName) =
        fromText $
            "You have asked me to filter the file: " <> fileName
            <> ", but I can only filter directories"
    showb (Can'tParse) =
        fromText $
            "Unable to parse the filter string - did you forget to close a bracket or a list?"
    showb (FailedToReadTree name) =
        fromText $
            "I could not read the requested source directory: " <> name
            <> ", does it exist?"

instance Show OtherException where
    show e = T.unpack $ toText $ showb e

instance Exception OtherException

data TSException a =
    Filter (FilterException a)
    | Other OtherException

instance Show (TSException a) where
    show (Filter x) = show $ showb x
    show (Other x) = show $ showb x

instance Typeable a => Exception (TSException a)

