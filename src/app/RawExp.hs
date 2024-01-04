{-# LANGUAGE OverloadedStrings #-}

module RawExp
  (
    NamedExp
    , RawExp(..)
    , getRange
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import TextShow

import ASTShow
import Lit

type NamedExp a = (T.Text, RawExp a)

-- Initial unparsed AST
data RawExp a =
    -- Parentheses
    RawPar a (RawExp a)
    -- Function application
    | RawApply a (NE.NonEmpty (RawExp a))
    -- List
    | RawList a [RawExp a]
    -- Literals
    | RawLit a Lit
    -- Syntax
    | RawIdent a T.Text
    | RawLet a [NamedExp a] (RawExp a)
    deriving Foldable

instance Functor RawExp where
    fmap f (RawPar a expr) = RawPar (f a) (fmap f expr)
    fmap f (RawApply a xs) = RawApply (f a) (fmap f <$> xs)
    fmap f (RawList a xs) = RawList (f a) (fmap f <$> xs)
    fmap f (RawLit a lit) = RawLit (f a) lit
    fmap f (RawIdent a txt) = RawIdent (f a) txt
    fmap f (RawLet a namedExprs expr) =
        let namedExprs' = (\(t, e) -> (t, fmap f e)) <$> namedExprs
        in RawLet (f a) namedExprs' (fmap f expr)

getRange :: RawExp a -> a
getRange (RawPar a _) = a
getRange (RawApply a _) = a
getRange (RawList a _) = a
getRange (RawLit a _) = a
getRange (RawIdent a _) = a
getRange (RawLet a _ _) = a

instance ASTShow (RawExp a) where
    astShow = astShow'

astShow' :: Int -> RawExp a -> Builder
astShow' i (RawPar _ x) =
    let i' = i + 1
    in indent i <> "( RawPar \n"
        <> astShow' i' x <> "\n"
        <> indent i <> ")"
astShow' i other =
    astShowRawApply i other

astShowRawApply :: Int -> RawExp a -> Builder
astShowRawApply i (RawApply _ xs) =
    let i' = i + 1
    in indent i <> "( RawApply \n"
        <> (intercalate "\n" (astShow' i' <$> NE.toList xs))
        <> (indent i) <> ")"
astShowRawApply i other =
    astShowRawList i other

astShowRawList :: Int -> RawExp a -> Builder
astShowRawList i (RawList _ xs) =
    let i' = i + 1
    in indent i <> "( RawList [\n"
        <> (intercalate "\n" (astShow' i' <$> xs))
        <> (indent i) <> "]"
astShowRawList i other =
    astShowRawLit i other

astShowRawLit :: Int -> RawExp a -> Builder
astShowRawLit i (RawLit _ x) =
    indent i <> "( RawLit " <> (astShow i x) <> " )"
astShowRawLit i other =
    astShowRawIdent i other

astShowRawIdent :: Int -> RawExp a -> Builder
astShowRawIdent i (RawIdent _ name) =
    indent i <> "( RawIdent " <> (fromText name) <> " )"
astShowRawIdent i other =
    astShowRawLet i other

astShowRawLet :: Int -> RawExp a -> Builder
astShowRawLet i (RawLet _ xs _) =
    indent i <> "( RawLet \n"
        <> (intercalate "\n" (showNamedExp i' <$> xs))
        <> (intercalate "\n" (showNamedExp i' <$> xs))
        <> (indent i) <> ")"
    where showNamedExp i'' (n, x) = (indent i'') <> (fromText n) <> " = " <> (astShow' i'' x)
          i' = i + 1

