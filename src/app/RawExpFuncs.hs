{-# LANGUAGE OverloadedStrings #-}

module RawExpFuncs
    (
        simplifyRawExp
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.List.NonEmpty( NonEmpty( (:|) ), (<|))

import Exceptions
import IsTrace
import InfixOperators
import RawExp

simplifyRawExp :: IsTrace trc => RawExp trc -> EitherF trc (RawExp trc)
simplifyRawExp expr = do
    namesRemoved <- removeNames M.empty expr
    let infixesResolved = resolveInfixes namesRemoved
    let parensRemoved = removeParens infixesResolved
    -- let identsConverted = leafToBaseTraverse identsToFunctionApplications parensRemoved
    return parensRemoved

setRawExpRange :: a -> RawExp a -> RawExp a
setRawExpRange a (RawPar _ x) = RawPar a x
setRawExpRange a (RawApply _ xs) = RawApply a xs
setRawExpRange a (RawList _ x) = RawList a x
setRawExpRange a (RawLit _ x) = RawLit a x
setRawExpRange a (RawIdent _ x) = RawIdent a x
setRawExpRange a (RawLet _ x y) = RawLet a x y

removeNames :: (IsTrace trc) =>
    M.Map T.Text (NamedExp trc) -> RawExp trc -> EitherF trc (RawExp trc)
removeNames nDefs (RawPar a x) = RawPar a <$> (removeNames nDefs x)
removeNames nDefs (RawApply a xs) = RawApply a <$> mapM (removeNames nDefs) xs
removeNames nDefs (RawList a xs) = RawList a <$> mapM (removeNames nDefs) xs
removeNames _ x@(RawLit _ _) = Right x
removeNames nDefs expr@(RawIdent a name) =
    case M.lookup name nDefs of
        Just x -> Right $ RawPar a (setRawExpRange a $ snd x)
        Nothing -> Right $ expr
removeNames existingNamedExps (RawLet a thisNamedExps expr) =
    let getName x = fst x
        duplicateWithinLet = snd $ L.foldl'
            (\acc x ->
                if elem (getName x) (getName <$> (fst acc))
                then (x:(fst acc), x:(snd acc))
                else (x:(fst acc), snd acc))
            ([], []) -- acc is ([NamedExp a], [NamedExp a])
            thisNamedExps
        allDuplicates = duplicateWithinLet
    in case allDuplicates of
        ((name, _):_) -> Left $ DuplicateName a name
        [] ->
            let mapList = (\ne -> (getName ne, ne)) <$> thisNamedExps
                thisMap = M.fromList mapList
                newMap = M.unions [thisMap, existingNamedExps]
            in removeNames newMap expr

resolveInfixes :: IsTrace a => RawExp a -> RawExp a
resolveInfixes x =
        baseToLeafTraverse resolveAllInfixesForExp x

resolveAllInfixesForExp :: IsTrace a => RawExp a -> RawExp a
resolveAllInfixesForExp x =
    L.foldl'
        (\acc op -> resolveInfixesForOperator op acc)
        x
        infixOperators

resolveInfixesForOperator :: IsTrace a => InfixOperator -> RawExp a -> RawExp a
resolveInfixesForOperator (op, LeftAssoc) (RawApply a xs) =
    let (expr:|xs') = NE.reverse xs
        resolved = resolveInfixesForOperator' op [] expr xs'
        xs'' = NE.reverse resolved
    in RawApply a xs''
resolveInfixesForOperator (op, RightAssoc) (RawApply a (expr:|xs)) =
    let xs' = resolveInfixesForOperator' op [] expr xs
    in RawApply a xs'
resolveInfixesForOperator _ x = x

resolveInfixesForOperator' :: IsTrace a =>
    T.Text -> [RawExp a] -> RawExp a -> [RawExp a] -> NonEmpty (RawExp a)
-- base case
resolveInfixesForOperator' _ acc expr [] = NE.reverse $ expr:|acc
-- special case
resolveInfixesForOperator' op acc expr@(RawIdent _ name) xs =
    if op == name
    then
        let arg0 = case acc of
                [] ->
                    let a' = L.foldl' (<>) (getRange expr) (getRange <$> xs)
                    in RawApply a' (expr:|xs)
                [x'] -> x'
                (x':xs') ->
                    let (x'':|xs'') = NE.reverse (x':|xs')
                        a' = L.foldl' (<>) (getRange x'') (getRange <$> xs'')
                    in RawApply a' (x'':|xs'')
            arg1 = case xs of
                [x'] -> x'
                (x':xs') ->
                    let a' = L.foldl' (<>) (getRange x') (getRange <$> xs')
                    in RawApply a' (x':|xs')
        in expr <| arg0 :| [arg1]
    else
        let (expr':xs') = xs
        in resolveInfixesForOperator' op (expr:acc) expr' xs'
-- continuation case
resolveInfixesForOperator' op acc expr (expr':xs) =
    resolveInfixesForOperator' op (expr:acc) expr' xs

removeParens :: RawExp a -> RawExp a
removeParens x =
    go $ leafToBaseTraverse go x
        where go (RawPar _ x') = x'
              go x' = x'

leafToBaseTraverse :: (RawExp a -> RawExp a) -> RawExp a -> RawExp a
leafToBaseTraverse f (RawPar a x) = f $ RawPar a $ (leafToBaseTraverse f x)
leafToBaseTraverse f (RawApply a xs) = f $ RawApply a $ leafToBaseTraverse f <$> xs
leafToBaseTraverse f (RawList a xs) = f $ RawList a $ leafToBaseTraverse f <$> xs
leafToBaseTraverse f (RawLit a x) = f $ RawLit a x
leafToBaseTraverse f (RawIdent a x) = f $ RawIdent a x
leafToBaseTraverse f (RawLet a namedExprs x) =
    let namedExprs' = (\(nm, expr) -> (nm, leafToBaseTraverse f expr)) <$> namedExprs
        x' = leafToBaseTraverse f x
    in f $ RawLet a namedExprs' x'

baseToLeafTraverse :: (RawExp a -> RawExp a) -> RawExp a -> RawExp a
baseToLeafTraverse = baseToLeafTraverse' False

baseToLeafTraverse' :: Bool -> (RawExp a -> RawExp a) -> RawExp a -> RawExp a
baseToLeafTraverse' False f expr = baseToLeafTraverse' True f (f expr)
baseToLeafTraverse' True f (RawPar a x) = RawPar a $ baseToLeafTraverse' False f x
baseToLeafTraverse' True f (RawApply a xs) = RawApply a $ baseToLeafTraverse' False f <$> xs
baseToLeafTraverse' True f (RawList a xs) = RawList a $ baseToLeafTraverse' False f <$> xs
baseToLeafTraverse' True _ (RawLit a x) = RawLit a x
baseToLeafTraverse' True _ (RawIdent a x) = RawIdent a x
baseToLeafTraverse' True f (RawLet a namedExprs x) =
    let namedExprs' = (\(nm, expr) -> (nm, baseToLeafTraverse' False f expr)) <$> namedExprs
        x' = leafToBaseTraverse f x
    in RawLet a namedExprs' x'
