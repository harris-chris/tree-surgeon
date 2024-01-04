{-# LANGUAGE MultiWayIf #-}

module ResolvedFuncs
    (
        convertToBool
        , simpleExpToResolved
        , getResolvedType
        , tryToResolvePartial
    ) where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T

import Exceptions
import FunctionDefinition
import FData
import IsTrace
import Lit
import SimpleExp
import Resolved
import ResolvedType
import TextShow

fileVariableName :: T.Text
fileVariableName = "file"

simpleExpToResolved :: IsTrace a => [ResolvedFuncDef a] -> FData -> SimpleExp a -> EitherF a (Resolved a)
simpleExpToResolved funcDefs fData (SimpleApply _ xs) =
    let xs' = mapM (simpleExpToResolved funcDefs fData) xs -- :: EitherF a (NE.NonEmpty (Resolved a))
    in (\(partialApp:fArgs) -> tryToResolvePartial fData partialApp fArgs) =<< NE.toList <$> xs'
simpleExpToResolved funcDefs fData (SimpleList a xs) =
    let xs' = mapM (simpleExpToResolved funcDefs fData) xs
    in RList a <$> xs'
simpleExpToResolved _ _ (SimpleLit a lit@(LString str))
    | str == fileVariableName = Right $ RFile a
    | otherwise = Right $ RLit a lit
simpleExpToResolved _ _ (SimpleLit a lit) = Right $ RLit a lit
simpleExpToResolved _ _ (SimpleIdent a "file") = Right $ RFile a
simpleExpToResolved funcDefs _ (SimpleIdent a name) =
    case L.find (\f -> funcName f == name) funcDefs of
        Just func -> Right $ RPartiallyApplied a func []
        Nothing -> Left $ FuncNameNotRecognized a name

tryToResolvePartial :: (IsTrace trc) =>
    FData -> Resolved trc -> [Resolved trc] -> EitherF trc (Resolved trc)
tryToResolvePartial fData (RPartiallyApplied a funcDef existingArgs) newArgs
    | (length allArgs) < (length $ expTypes) =
        Right $ RPartiallyApplied a funcDef allArgs
    | (length allArgs) == (length $ expTypes) =
        let actualArgTypes = mapM getResolvedType allArgs
            a' = L.foldl' (<>) a (getRange <$> newArgs)
            validated = validateArgs a' fName expTypes <$> actualArgTypes
        in case validated of
            Left err -> Left err
            Right [] -> (funcApply funcDef) fData allArgs
            Right (x:_) -> Left $ x
    | (length allArgs) > (length $ expTypes) =
        Left $ FuncWrongNumArgs a fName (length allArgs) (length expTypes)
    where
        allArgs = existingArgs ++ newArgs
        expTypes = expectedTypes funcDef
        fName = funcName funcDef
tryToResolvePartial _ _ _ = error "Tried to resolve non-partial"

validateArgs :: a -> T.Text -> [ResolvedType] -> [ResolvedType] -> [FilterException a]
validateArgs trc fName expectedTypes actualTypes =
    let fold_f = (\(generics, prevExceptions) (n, act, expr) ->
                    case validateArg generics act expr of
                        (generics', Nothing) -> (generics', prevExceptions)
                        (generics', Just (actual, expected)) ->
                            let newEx = FuncArgWrongType trc fName n actual expected
                            in (generics', newEx:prevExceptions))
        folded = L.foldl'
                    fold_f
                    (M.empty, []) -- acc is (Map Int ResolvedType, [FilterException a])
                    (zip3 [0..(length actualTypes)] actualTypes expectedTypes)
    in snd folded

validateArg ::
    M.Map T.Text ResolvedType ->
    ResolvedType ->
    ResolvedType ->
    (M.Map T.Text ResolvedType, Maybe (ResolvedType, ResolvedType))
validateArg _ (TGeneric _) _ = error "actualType was a generic"
validateArg genericsMap actual expected
    | actual == expected' = (genericsMap', Nothing)
    | otherwise = (genericsMap', Just (actual, expected'))
    where (genericsMap', expected') = replaceGenerics genericsMap expected

convertToBool :: Semigroup a => Resolved a -> EitherF a Bool
convertToBool (RLit _ (LBool bl)) = Right $ bl
convertToBool x =
    case getResolvedType x of
        Left e -> Left e
        Right typ -> Left $ Can'tResolveAsBool (getRange x) (T.pack "placeholder") typ

getResolvedType :: Resolved a -> EitherF a ResolvedType
getResolvedType (RPartiallyApplied _ funcDef existingArgs) =
    let remainingArgTypes = L.drop (length existingArgs) (expectedTypes funcDef)
    in Right $ TPartiallyApplied remainingArgTypes
getResolvedType (RList _ []) = Right $ TList TAny
getResolvedType (RList a xs) =
    let allTypes = mapM getResolvedType xs
    in case allTypes of
        (Right []) -> Right $ TList TAny
        (Right (x':xs')) ->
            if all (\y -> y == x') xs'
            then Right $ TList x'
            else Left $ ListNotHeterogeneous a $ showt <$> (x':xs')
        (Left err) -> Left err
getResolvedType (RLit _ (LBool _)) = Right TBool
getResolvedType (RLit _ (LString _)) = Right TString
getResolvedType (RLit _ (LInteger _)) = Right TInteger
getResolvedType (RFile _) = Right TFile
