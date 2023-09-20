module ASTFuncs
  (
    deNameExp
    , getMatcher
    , resolve
    , simplifyExp
    , Matcher
  ) where

import qualified Data.ByteString.Lazy.Char8 as BS

import AST
import Functions
import TSException

type Matcher = FData -> Either TSException Bool

deNameExp :: (Show a, Eq a) => [NamedExp a] -> Exp a -> Either TSException (Exp a)
deNameExp nDefs (And a x y) = And a <$> (deNameExp nDefs x) <*> (deNameExp nDefs y)
deNameExp nDefs (Not a x) = Not a <$> (deNameExp nDefs x)
deNameExp nDefs (Or a x y) = Or a <$> (deNameExp nDefs x) <*> (deNameExp nDefs y)
deNameExp nDefs (Eqs a x y) = Eqs a <$> (deNameExp nDefs x) <*> (deNameExp nDefs y)
deNameExp _ x@(ELit _ _) = Right x
-- let matchesFunc = (==) in matchesFunc (basename file) "myFile"
-- let matchesBool = (==) (basename file) "myFile" in matchesBool
-- In this second one, `matchesBool` is a function that takes no arguments? or a Bool?
deNameExp nDefs (EFunc a v@(VarName _ nmStr) args) =
    let matches = filter (\n -> (getNameOnly $ fst n) == nmStr) nDefs
    in case matches of
        [] ->
            Right $ EFunc a v args
        -- if the variable is a user-defined (potentially partially applied) function
        -- ie, let isOne = (== 1) in isOne (filesize file)
        [(_, EFunc a' v' args')] ->
            let combinedArgs = args' ++ args
            in Right $ EFunc a v' combinedArgs
        [x] ->
            Left $ NotAFunction (BS.unpack nmStr) (show <$> args)
deNameExp nDefs (EPar a x) = EPar a <$> (deNameExp nDefs x)
deNameExp nDefs dec@(ELet _ namedExps exp) =
    case namesMatchExp nDefs namedExps of
        [] -> let nDefs' = namedExps ++ nDefs
              in deNameExp nDefs' exp
        d:_ -> Left $ DuplicateName (show $ getNameOnly $ fst d) (show $ snd d)

namesMatchExp :: [NamedExp a] -> [NamedExp a] -> [NamedExp a]
namesMatchExp xs ys = namesMatchExp' xs ys []

namesMatchExp' :: [NamedExp a] -> [NamedExp a] -> [NamedExp a] -> [NamedExp a]
namesMatchExp' (x:xs) ys acc =
    namesMatchExp' xs ys $ acc
    ++ filter (\n -> (getNameOnly $ fst n) == (getNameOnly $ fst x)) ys
namesMatchExp' [] ys acc = acc

-- Resolve all the remaining functions; since we have run deName prior to this point,
-- these functions should only be the built-in functions
resolve :: (Show a, Eq a) => FData -> Exp a -> Either TSException (Lit a)
resolve fData (And a x y) =
    case ((resolve fData x), (resolve fData y)) of
        (Right (LBool a x'), Right (LBool b y')) -> Right $ LBool a (x' && y')
        (Right x', Right (LBool _ _)) -> Left $ Can'tApplyLogicalToNonBool (show x')
        (Right (LBool _ _), y') -> Left $ Can'tApplyLogicalToNonBool (show y')
        (Left err, _) -> Left err
        (_, Left err) -> Left err
resolve fData (Not a x) =
    let x' = resolve fData x
    in case x' of
        (Right (LBool y bl)) -> Right $ LBool y (not bl)
        (Right y) -> Left $ Can'tApplyLogicalToNonBool $ show y
        (Left err) -> Left err
resolve fData (Or a x y) =
    case ((resolve fData x), (resolve fData y)) of
        (Right (LBool a x'), Right (LBool b y')) -> Right $ LBool a (x' || y')
        (Right x', Right (LBool _ _)) -> Left $ Can'tApplyLogicalToNonBool (show x')
        (Right (LBool _ _), y') -> Left $ Can'tApplyLogicalToNonBool (show y')
        (Left err, _) -> Left err
        (_, Left err) -> Left err
resolve fData (Eqs a x y) =
    case ((resolve fData x), (resolve fData y)) of
        (Right (LBool a x'), Right (LBool b y')) -> Right $ LBool a (x' == y')
        (Right (LString a x'), Right (LString b y')) -> Right $ LBool a (x' == y')
        (Right (LFile a), Right (LFile b)) -> Right $ LBool a True
        (Right x', Right y') -> Left $ Can'tCompare (show x') (show y')
        (Left err, _) -> Left err
        (_, Left err) -> Left err
resolve _ (ELit _ lit) = Right lit
resolve fData (EFunc a (VarName _ fName) args) =
    let args' = mapM (resolve fData) args
    in resolveFunc fData (BS.unpack fName) =<< args'
resolve fData (EPar a x) = resolve fData x
resolve _ (ELet _ _ _) = error "Let found in resolve"

simplifyExp :: (Show a, Eq a) => Exp a -> Either TSException (Exp a)
simplifyExp exp = do
    deNamed <- deNameExp [] exp
    return deNamed

getMatcher :: (Show a, Eq a) => Exp a -> FData -> Either TSException Bool
getMatcher exp fData = do
    resolved <- resolve fData exp
    asBool <- convertToBool resolved
    return asBool

convertToBool :: (Show a) => Lit a -> Either TSException Bool
convertToBool (LBool _ bl) = Right $ bl
convertToBool lit = Left $ Can'tResolveAsBool (show lit)

