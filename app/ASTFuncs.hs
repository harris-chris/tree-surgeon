module ASTFuncs
  (
    getMatcher
  ) where

import qualified Data.ByteString.Lazy.Char8 as BS

import AST
import Functions
import TSException

deNameExp :: (Show a, Eq a) => [NamedExp a] -> Exp a -> Either ExpException (Exp a)
deNameExp nDefs (And a x y) = And a <$> (deNameExp nDefs x) <*> (deNameExp nDefs y)
deNameExp nDefs (Not a x) = Not a <$> (deNameExp nDefs x)
deNameExp nDefs (Or a x y) = Or a <$> (deNameExp nDefs x) <*> (deNameExp nDefs y)
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
resolve :: (Show a, Eq a) => FData -> Exp a -> Either RuntimeException Bool
resolve fData (And a x y) =
    case ((resolve fData x), (resolve fData y)) of
        (Right fx, Right fy) -> Right $ fx && fy
        (Left err, _) -> Left err
        (_, Left err) -> Left err
resolve fData (Not a x) =
    let x' = resolve fData x
    in not <$> x'
resolve fData (Or a x y) =
    case ((resolve fData x), (resolve fData y)) of
        (Right fx, Right fy) -> Right $ fx || fy
        (Left err, _) -> Left err
        (_, Left err) -> Left err
resolve _ (ELit _ (LBool _ bl)) = Right bl
resolve _ (ELit _ lit) = Left $ Can'tResolveAsBool $ show lit
resolve fData (EFunc a (VarName _ fName) args) =
    let funcResolved = resolveFunc fData (BS.unpack fName) args
    in resolve <$> funcResolved fData
resolve fData (EPar a x) = resolve fData x
resolve _ (ELet _ _ _) = error "Let found in resolve"

