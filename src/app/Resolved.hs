{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Resolved
    (
        Resolved(..)
        , ResolvedFuncDef
        , getRange
    ) where

import TextShow

import ASTShow
import FunctionDefinition
import ResolvedType
import Lit

type ResolvedFuncDef a = FunctionDefinition ResolvedType Resolved a

data Resolved a =
    RPartiallyApplied a (ResolvedFuncDef a) [Resolved a]
    | RList a [Resolved a]
    | RLit a Lit
    | RFile a

getRange :: Resolved a -> a
getRange (RPartiallyApplied a _ _) = a
getRange (RList a _) = a
getRange (RLit a _) = a
getRange (RFile a) = a

instance Eq (Resolved a) where
    (RPartiallyApplied _ funcDef args) == (RPartiallyApplied _ funcDef' args') = funcDef == funcDef' && args == args'
    (RList _ xs) == (RList _ xs') = xs == xs'
    (RLit _ x) == (RLit _ x') = x == x'
    (RFile _) == (RFile _) = True
    _ == _ = False

instance ASTShow (Resolved a) where
    astShow = astShow'

astShow' :: Int -> Resolved a -> Builder
astShow' i (RPartiallyApplied _ funcDef args) =
    let i' = i + 1
    in indent i <> "( RPartiallyApplied " <> (fromText $ funcName funcDef) <> "\n"
        <> (intercalate "\n" (astShow' i' <$> args)) <> "\n"
        <> indent i <> ")"
astShow' i other =
    astShowRList i other

astShowRList :: Int -> Resolved a -> Builder
astShowRList i (RList _ xs) =
    let i' = i + 1
    in indent i <> "( RList \n"
        <> (intercalate "\n" (astShow' i' <$> xs))
        <> (indent i) <> ")"
astShowRList i other =
    astShowRLit i other

astShowRLit :: Int -> Resolved a -> Builder
astShowRLit i (RLit _ x) =
    indent i <> "( RLit " <> (astShow i x) <> " )"
astShowRLit i other =
    astShowRFile i other

astShowRFile :: Int -> Resolved a -> Builder
astShowRFile i (RFile _) =
    indent i <> "( RFile )"

