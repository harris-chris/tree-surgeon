module ASTFuncs
  (
    getMatcher
  ) where

import AST
import Functions
import TSException

deName :: (Show a, Eq a) => [NamedExpr a] -> Exp a -> Either TSException (Exp a)
deName nDefs (And a x y) = And a <$> (deName nDefs x) <*> (deName nDefs y)
deName nDefs (Not a x) = Not a <$> (deName nDefs x)
deName nDefs (Or a x y) = Or a <$> (deName nDefs x) <*> (deName nDefs y)
deName nDefs (EFunc a v@(VarName _ nmStr) args) =
    let matches = filter (\n -> (getNameOnly $ fst n) == nmStr) nDefs
    in case matches of
        [] ->
            EFunc a v <$> mapM (deName nDefs) args
        [(_, EFunc a' v' args')] ->
            let combinedArgs = args' ++ args
            in EFunc a v' <$> mapM (deName nDefs) combinedArgs
        [x] ->
            Left $ NotAFunction (BS.unpack nmStr) (show <$> args)
deName nDefs (EList a xs) = EList a <$> mapM (deName nDefs) xs
deName nDefs exp@(EString _ _) = Right $ exp
deName nDefs (EPar a x) = EPar a <$> (deName nDefs x)
deName nDefs dec@(Let _ namedExprs exp) =
    case namesMatch nDefs namedExprs of
        [] -> let nDefs' = namedExprs ++ nDefs
              in deName nDefs' exp
        d:_ -> Left $ DuplicateName (show $ getNameOnly $ fst d) (show $ snd d)
deName nDefs exp = Right $ exp

namesMatch :: [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a]
namesMatch xs ys = namesMatch' xs ys []

namesMatch' :: [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a]
namesMatch' (x:xs) ys acc =
    namesMatch' xs ys $ acc
    ++ filter (\n -> (getNameOnly $ fst n) == (getNameOnly $ fst x)) ys
namesMatch' [] ys acc = acc

-- Resolve literal-related functions, including `basename` and others related to the
-- `file` variable.
resolveLit :: (Show a, Eq a) => Lit a -> Either TSException (FData -> Lit a)
resolveLit x@(LBool _ _) = Right $ \_ -> x
resolveLit (LList a xs) = EList a <$> mapM resolveLit xs
resolveLit (LPar a x) = LPar a <$> (resolveLit x)
resolveLit x@(LString _ _) = Right $ \_ -> x
resolveLit (LFunc a v@(VarName _ nmStr) args) =
    let args' = mapM resolveLit args
    in parseLitFunc name <$> args'
resolveLit x@(LVar _ _) = Right $ \_ -> x

-- Resolve all the remaining functions; since we have run deName prior to this point,
-- these functions should only be the built-in functions
getMatcher :: (Show a, Eq a) => Exp a -> Either TSException (FData -> Bool)
getMatcher (And a x y) = And <$> (getMatcher x) <*> (getMatcher y)
    case ((getMatcher x), (getMatcher y)) of
        (Right fx, Right fy) -> Right $ \d -> fx d && fy d
        (Left err, _) -> Left err
        (_, Right err) -> Right err
getMatcher (Not a x) =
    let invertMatcher = \matcher -> (\fn fsObj -> not $ matcher fn fsObj)
    in invertMatcher <$> getMatcher x
getMatcher (Or a x y) =
    case ((getMatcher x), (getMatcher y)) of
        (Right fx, Right fy) -> Right $ \d -> fx d || fy d
        (Left err, _) -> Left err
        (_, Right err) -> Right err
getMatcher (EFunc a (VarName _ fName) args)
    let args' = mapM resolveLit args
    in parseFunc name <$> args'
        -- here we only deal with the functions that DO resolve to Bool
        where parseFunc name args'
            | name == "==" = eqsFunc args'
            | name == "basename" = basenameFunc args'
getMatcher (EPar a x) = EPar a <$> getMatcher x
getMatcher (Let _ _ _) = error "Let found in getMatcher"

resolve :: (Show a, Eq a) => Exp a -> Either TSException (FData -> Bool)
resolve exp =
    let expE = deName [] exp
    in getMatcher =<< expE

