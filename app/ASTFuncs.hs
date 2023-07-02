module ASTFuncs
  (
    deName
    , deFunc
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

-- Resolve all the remaining functions; since we have run deName prior to this point,
-- these functions should only be the built-in functions
deFunc :: (Show a, Eq a) => FsObjData -> Exp a -> Either TSException (Exp a)
deFunc fsObjData (And a x y) = And a <$> (deFunc fsObjData x) <*> (deFunc fsObjData y)
deFunc fsObjData (Not a x) = Not a <$> (deFunc fsObjData x)
deFunc fsObjData (Or a x y) = Or a <$> (deFunc fsObjData x) <*> (deFunc fsObjData y)
deFunc fsObjData (EFunc a (VarName _ fName) args)
    let args' = mapM (deFunc fsObjData) args
    in parseFunc name <$> args'
        where parseFunc name args'
            | name == "==" = eqsFunc args'
            | name == "basename" = basenameFunc args'
deFunc fsObjData (EList a xs) = EList a <$> mapM (deFunc fsObjData) xs
deFunc fsObjData exp@(EString _ _) = error "Literal is found outside a function"
deFunc fsObjData (EPar a x) = EPar a <$> (deFunc fsObjData x)
deFunc fsObjData dec@(Let _ namedExprs exp) = error "Let found in deFunc"

namesMatch :: [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a]
namesMatch xs ys = namesMatch' xs ys []

namesMatch' :: [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a]
namesMatch' (x:xs) ys acc =
    namesMatch' xs ys $ acc
    ++ filter (\n -> (getNameOnly $ fst n) == (getNameOnly $ fst x)) ys
namesMatch' [] ys acc = acc

-- resolveBuiltins :: (Eq a, Show a) => Exp a -> Either TSException Bool
-- resolveBuiltins (And _ x y) =
--     case ((resolveBuiltins x), (resolveBuiltins y)) of
--         (Right bx, Right by) -> Right $ bx && by
--         (Left err, _) -> Left err
--         (_, Left err) -> Left err
-- resolveBuiltins (Not _ exp) =
--     not <$> resolveBuiltins exp
-- resolveBuiltins (Or _ x y) =
--     case ((resolveBuiltins x), (resolveBuiltins y)) of
--         (Right bx, Right by) -> Right $ bx || by
--         (Left err, _) -> Left err
--         (_, Left err) -> Left err
-- resolveBuiltins (EList _ xs) = error "Encountered EList"
-- resolveBuiltins (EString _ x) = error "Encountered EString"
-- resolveBuiltins (EPar _ x) = resolveBuiltins x
-- resolveBuiltins (Let _ namedExprs exp) = error "Encountered Let"
-- resolveBuiltins exp = Left $ Couldn'tParse $ show exp

