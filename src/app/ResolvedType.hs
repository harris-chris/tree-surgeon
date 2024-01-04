module ResolvedType
    (
        ResolvedType(..)
        , replaceGenerics
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

import TextShow

data ResolvedType =
    TPartiallyApplied [ResolvedType] --  a list of remaining types needed
    | TList ResolvedType -- the type of the list elements
    | TBool
    | TString
    | TInteger
    | TFile
    | TAny
    | TGeneric T.Text
    | TUnion [ResolvedType]

instance Eq ResolvedType where
    TPartiallyApplied xs == TPartiallyApplied xs' = xs == xs'
    TList x == TList x' = x == x'
    TBool == TBool = True
    TString == TString = True
    TInteger == TInteger = True
    TFile == TFile = True
    x == TUnion xs = elem x xs
    TAny == _ = True
    _ == TAny = True
    _ == _ = False


-- The first element of the tuple is an updated map containing new generics that were found
-- If a generic is not part of the generics map, this is fine; it is replaced with TAny
replaceGenerics ::
    M.Map T.Text ResolvedType -> ResolvedType -> (M.Map T.Text ResolvedType, ResolvedType)
replaceGenerics genericsMap (TPartiallyApplied argTypes) =
    let (genericsMap', types) = L.foldl' foldF (genericsMap, []) argTypes
    in (genericsMap', TPartiallyApplied (reverse types))
    where foldF (gm, ts) x =
                          let (gm', t) = replaceGenerics gm x
                          in (gm', t:ts)
replaceGenerics genericsMap (TList x) = TList <$> replaceGenerics genericsMap x
replaceGenerics genericsMap (TGeneric name) =
    case M.lookup name genericsMap of
        Just replacementType -> (genericsMap, replacementType)
        Nothing -> (genericsMap, TAny)
replaceGenerics genericsMap x = (genericsMap, x)

instance TextShow ResolvedType where
    showb (TPartiallyApplied types) =
        "Partially applied function which expects arguments " <>
        L.foldl' (\acc x -> acc <> "( " <> (showb x) <> " )") "" types
    showb (TList typ) =
        "List with elements of type " <> (showb typ)
    showb (TBool) =
        fromText $ "Bool"
    showb (TString) =
        fromText $ "String"
    showb (TInteger) =
        fromText $ "Integer"
    showb (TFile) =
        fromText $ "the special file variable"
    showb (TAny) =
        fromText $ "Any type"
    showb (TGeneric x) =
        fromText $ "Generic argument number " <> x

