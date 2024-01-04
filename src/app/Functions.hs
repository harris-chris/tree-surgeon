{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Functions
  (
    functions
  ) where

import qualified Data.Text as T

import FData
import IsTrace
import Lit
import Resolved
import ResolvedFuncs
import ResolvedType
import FunctionDefinition
import Exceptions

functions :: [ResolvedFuncDef a]
functions = [
        FunctionDefinition "|" [TBool, TBool] orFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition "&" [TBool, TBool] andFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition "==" [TGeneric "X",TGeneric "X"] eqsFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition "<" [TInteger, TInteger] ltFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition ">" [TInteger, TInteger] gtFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition "<=" [TInteger, TInteger] lteFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition ">=" [TInteger, TInteger] gteFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition "-" [TInteger, TInteger] minusFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition "+" [TInteger, TInteger] plusFunc -- Is infix, in InfixOperators.hs
        , FunctionDefinition "elem" [TGeneric "X", TList (TGeneric "X")] inFunc
        , FunctionDefinition "!" [TBool] notFunc
        , FunctionDefinition "all" [TPartiallyApplied [TGeneric "X"], TList (TGeneric "X")] allFunc
        , FunctionDefinition "any" [TPartiallyApplied [TGeneric "X"], TList (TGeneric "X")] anyFunc
        , FunctionDefinition "basename" [TFile] basenameFunc
        , FunctionDefinition "isFile" [TFile] isFileFunc
        , FunctionDefinition "isDir" [TFile] isDirFunc
        , FunctionDefinition "length" [TUnion[TList (TAny), TString]] lengthFunc
        , FunctionDefinition "parents" [TFile] parentsFunc
        , FunctionDefinition "startsWith" [TString, TString] startsWithFunc
        , FunctionDefinition "endsWith" [TString, TString] endsWithFunc
        , FunctionDefinition "occursIn" [TString, TString] occursInFunc
        , FunctionDefinition "map" [TPartiallyApplied [TGeneric "X"], TList (TGeneric "X")] mapFunc
    ]

orFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
orFunc _ [RLit a (LBool False), RLit _ (LBool False)] = Right $ RLit a $ LBool False
orFunc _ [RLit a (LBool _), RLit _ (LBool _)] = Right $ RLit a $ LBool True

andFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
andFunc _ [RLit a (LBool True), RLit _ (LBool True)] = Right $ RLit a $ LBool True
andFunc _ [RLit a (LBool _), RLit _ (LBool _)] = Right $ RLit a $ LBool False

eqsFunc :: IsTrace trc => (IsTrace trc) => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
eqsFunc _ [x, y] = Right $ RLit (getRange x) $ LBool (x == y)

ltFunc :: IsTrace trc => (IsTrace trc) => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
ltFunc _ [RLit a (LInteger x), RLit _ (LInteger y)] = Right $ RLit a $ LBool (x < y)

gtFunc :: IsTrace trc => (IsTrace trc) => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
gtFunc _ [RLit a (LInteger x), RLit _ (LInteger y)] = Right $ RLit a $ LBool (x > y)

lteFunc :: IsTrace trc => (IsTrace trc) => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
lteFunc _ [RLit a (LInteger x), RLit _ (LInteger y)] = Right $ RLit a $ LBool (x <= y)

gteFunc :: IsTrace trc => (IsTrace trc) => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
gteFunc _ [RLit a (LInteger x), RLit _ (LInteger y)] = Right $ RLit a $ LBool (x >= y)

plusFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
plusFunc _ [RLit a (LInteger x), RLit _ (LInteger y)] =
    Right $ RLit a $ LInteger $ x + y

minusFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
minusFunc _ [RLit a (LInteger x), RLit _ (LInteger y)] =
    Right $ RLit a $ LInteger $ x - y

inFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
inFunc _ [x, RList a xs] =
    Right $ RLit a $ LBool $ elem x xs

notFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
notFunc _ [RLit a (LBool True)] = Right $ RLit a $ LBool False
notFunc _ [RLit a (LBool False)] = Right $ RLit a $ LBool True

allFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
allFunc fData [f@(RPartiallyApplied _ _ _), RList a xs] =
    let asBools = mapM (\x -> convertToBool =<< tryToResolvePartial fData f [x]) xs
    in (\xs' -> RLit a $ LBool $ all id xs') <$> asBools

anyFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
anyFunc fData [f@(RPartiallyApplied _ _ _), RList a xs] =
    let asBools = mapM (\x -> convertToBool =<< tryToResolvePartial fData f [x]) xs
    in (\xs' -> RLit a $ LBool $ any id xs') <$> asBools

basenameFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
basenameFunc fData [RFile a] =
    Right $ RLit a $ LString $ basename fData

isFileFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
isFileFunc fData [RFile a] =
    Right $ RLit a $ LBool $ (fileType fData) == FileFileType

isDirFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
isDirFunc fData [RFile a] =
    Right $ RLit a $ LBool $ (fileType fData) == DirFileType

parentsFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
parentsFunc fData [RFile a] =
    Right $ RList a $ ((RLit a) . LString) <$> (parents fData)

startsWithFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
startsWithFunc _ [RLit a (LString searchStr), RLit _ (LString str)] =
    Right $ RLit a $ LBool $ T.isPrefixOf searchStr str

endsWithFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
endsWithFunc _ [RLit a (LString searchStr), RLit _ (LString str)] =
    Right $ RLit a $ LBool $ T.isSuffixOf searchStr str

occursInFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
occursInFunc _ [RLit a (LString searchStr), RLit _ (LString str)] =
    Right $ RLit a $ LBool $ T.isInfixOf searchStr str

lengthFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
lengthFunc _ [unionArg] =
    case unionArg of
        RList a xs -> Right $ RLit a $ LInteger $ length xs
        RLit a (LString str) -> Right $ RLit a $ LInteger $ T.length str

mapFunc :: IsTrace trc => FData -> [Resolved trc] -> EitherF trc (Resolved trc)
mapFunc fData [f@(RPartiallyApplied _ _ _), RList a xs] =
    let mapped = mapM (\x -> tryToResolvePartial fData f [x]) xs
    in RList a <$> mapped

