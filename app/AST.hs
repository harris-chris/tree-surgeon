{-# LANGUAGE DeriveTraversable  #-}

module AST
  (
    Exp(..)
    , NamedExpr(..)
    , VarName(..)
    , FsObjData(..)
    , IsFilePath(..)
    , TSException(..)
  ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Char8 (isInfixOf)
import Control.Monad.State.Lazy
import Data.List (intercalate)
import Data.Traversable
import Debug.Trace
import System.FilePath
import System.Directory.Tree

import Functions

data TSException =
    Couldn'tLex String
    | Couldn'tParse String
    | DuplicateName String String
    | FuncArgs String [String]
    | FuncWrongNumArgs String Integer Integer
    | NotAFunction String [String]
    | UnrecognizedName [String] String
    deriving (Eq)

instance Exception TSException

instance Show TSException where
    show (Couldn'tLex expStr) =
        "Error in expression:\n" <> expStr
    show (Couldn'tParse expStr) =
        "Error in expression:\n" <> expStr
    show (FuncCalledWithWrongNumArgs funcName actualNum expectedNum) =
        "Error:\n" <>
        "Function " <> funcName <>
        " expects " <> show expectedNum <>
        " arguments, but has been called with " <> show actualNum
    show (UnrecognizedName varsInScope name) =
        "Error: Unrecognized name " <> name
        <> "; have names ["
        <> (intercalate ", " varsInScope)
        <> "] in scope"
    show (DuplicateName dec name) =
        "Error: name " <> name
        <> " in declaration " <> dec
        <> " already exists"
    show (NotAFunction name args) =
        "Error: name " <> name
        <> " is being applied to arguments "
        <> (intercalate " " args)
        <> " but is not a function"

data FsObjData =
    FileData {
        basename :: String
        , parents :: [ByteString]
        }
    deriving (Eq, Show, Ord)

class IsFilePath a where
    toFilePath :: a -> FilePath

instance IsFilePath FsObjData where
    toFilePath (FileData basename pts) = joinPath $ basename:(BS.unpack <$> pts)

getNameOnly :: VarName a -> ByteString
getNameOnly (VarName _ bs) = bs

type NamedExpr a = (VarName a, Exp a)

data Exp a =
    -- Logical operators
    And a (Exp a) (Exp a)
    | Not a (Exp a)
    | Or a (Exp a) (Exp a)
    -- Function
    | EFunc a (VarName a) [Exp a]
    -- Literals
    | EList a [Exp a]
    | EString a ByteString
    -- Syntax
    | EPar a (Exp a)
    | Let a [NamedExpr a] (Exp a)
    deriving (Foldable, Functor, Eq, Show, Traversable)

-- deName then deFunc gets us here
data ResolvedExp a =
    And a (ResolvedExp a) (ResolvedExp a)
    | Not a (ResolvedExp a)
    | Or a (ResolvedExp a) (ResolvedExp a)
    -- Literals
    | EList a [ResolvedExp a]
    | EString a ByteString
    | EFile a FsObjData
    -- Syntax
    | EPar a (ResolvedExp a)
    deriving (Foldable, Functor, Eq, Show, Traversable)

data VarName a
    = VarName a ByteString
    deriving (Foldable, Functor, Eq, Show, Traversable)

-- resolveFileVar :: (Show a, Eq a) => Exp a -> FsObjData -> Either TSException (Exp a)
-- resolveFileVar exp fsObjData = deName' [(VarName $ BS.pack "file", ObjData fsObjData)] exp

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
            | name == "==" = funcEqs args'
            | name == "basename" = funcName args'
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

