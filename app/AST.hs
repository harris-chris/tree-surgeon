{-# LANGUAGE DeriveTraversable  #-}

module AST
  (
    Exp(..)
    , NamedExpr(..)
    , VarName(..)
    , FsObjData(..)
    , IsExp(..)
    , IsFilePath(..)
    , TreeSurgeonException(..)
    , Matcher
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

type Matcher = (FsObjData -> Bool)
type MatcherE a = Either TreeSurgeonException Matcher
type NameMatcherFunc = (ByteString -> ByteString -> Bool)

data TreeSurgeonException
    = Couldn'tParse String
    | Couldn'tLex String
    | NameMatcherNeedsString String
    | AncestorNameIsNeedsString String
    | CanOnlyFilterDirectory String
    | UnrecognizedName [ByteString] String
    | DuplicateName String String
    deriving (Eq)

instance Exception TreeSurgeonException

instance Show TreeSurgeonException where
    show (Couldn'tLex expStr) =
        "Error in expression:\n" <> expStr
    show (Couldn'tParse expStr) =
        "Error in expression:\n" <> expStr
    show (NameMatcherNeedsString exp) =
        "Error:\n" <> (show exp) <>
        "\nname matching functions must be passed string or list of strings"
    show (AncestorNameIsNeedsString exp) =
        "Error:\n" <> (show exp) <>
        "\nancestorNameIs must be passed string or list of strings"
    show (CanOnlyFilterDirectory fpath) =
        "Error: unable to filter " <> (show fpath) <>
        "; it is a file, not a directory"
    show (UnrecognizedName varsInScope name) =
        "Error: Unrecognized name " <> name
        <> "; have names ["
        <> (intercalate ", " $ BS.unpack <$> varsInScope)
        <> "] in scope"
    show (DuplicateName dec name) =
        "Error: name " <> name
        <> " in declaration " <> dec
        <> " already exists"

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

class Show exp => IsExp exp where
    getMatcher :: exp -> MatcherE exp
    deName :: exp -> Either TreeSurgeonException exp

data VarName
    = VarName ByteString
    deriving (Eq, Show)

getNameOnly :: VarName -> ByteString
getNameOnly (VarName bs) = bs

type NamedExpr a = (VarName, Exp a)

-- data Dec a
--   = Let (VarName a) (Exp a) (Exp a)
--   deriving (Foldable, Show)

data Exp a =
    Or a (Exp a) (Exp a)
    | And a (Exp a) (Exp a)
    | Not a (Exp a)
    | AncestorNameIs a (Exp a)
    | AncestorNameStartsWith a (Exp a)
    | AncestorNameEndsWith a (Exp a)
    | AncestorNameContains a (Exp a)
    | NameIs a (Exp a)
    | NameStartsWith a (Exp a)
    | NameEndsWith a (Exp a)
    | NameContains a (Exp a)
    | All a
    | None a
    | EVar a VarName
    | EPar a (Exp a)
    | EString a ByteString
    | EList a [Exp a]
    | Let a [NamedExpr a] (Exp a)
    | ObjData FsObjData -- we replace the `file` var with this
    deriving (Foldable, Functor, Show, Traversable)

resolveFileVar :: (Show a, Eq a) => Exp a -> FsObjData -> Either TreeSurgeonException (Exp a)
resolveFileVar exp fsObjData = deName' [(VarName $ BS.pack "file", ObjData fsObjData)] exp

deNameExp :: (Show a, Eq a) => Exp a -> Either TreeSurgeonException (Exp a)
deNameExp exp = deName' [] exp

deName' :: (Show a, Eq a) => [NamedExpr a] -> Exp a -> Either TreeSurgeonException (Exp a)
deName' nameDefs dec@(Let _ namedExprs exp) =
    case namesMatch nameDefs namedExprs of
        [] -> let nameDefs' = namedExprs ++ nameDefs
              in deName' nameDefs' exp
        d:_ -> Left $ DuplicateName (show $ getNameOnly $ fst d) (show $ snd d)
deName' nameDefs (EVar _ name@(VarName nmStr)) =
    let matches = filter (\n -> (getNameOnly $ fst n) == nmStr) nameDefs
    in case matches of
        [] -> let varsInScope = (getNameOnly . fst) <$> nameDefs
              in Left $ UnrecognizedName varsInScope (show nmStr)
        [x] -> let expr = snd x
               in deName' nameDefs expr
deName' nameDefs (Or a x y) = Or a <$> (deName' nameDefs x) <*> (deName' nameDefs y)
deName' nameDefs (And a x y) = And a <$> (deName' nameDefs x) <*> (deName' nameDefs y)
deName' nameDefs (Not a x) = Not a <$> (deName' nameDefs x)
deName' nameDefs (EPar a x) = EPar a <$> (deName' nameDefs x)
deName' nameDefs (EList a xs) = EList a <$> mapM (deName' nameDefs) xs
deName' nameDefs exp = Right $ exp

namesMatch :: [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a]
namesMatch xs ys = namesMatch' xs ys []

namesMatch' :: [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a] -> [NamedExpr a]
namesMatch' (x:xs) ys acc =
    namesMatch' xs ys $ acc
    ++ filter (\n -> (getNameOnly $ fst n) == (getNameOnly $ fst x)) ys
namesMatch' [] ys acc = acc

instance (Eq a, Show a) => IsExp (Exp a) where
    deName = deNameExp
    getMatcher (Or _ x y) =
        case ((getMatcher x), (getMatcher y)) of
            (Right fx, Right fy) -> Right $ \objD -> fx objD || fy objD
            (Left err, _) -> Left err
            (_, Right err) -> Right err
    getMatcher (And _ x y) =
        case ((getMatcher x), (getMatcher y)) of
            (Right fx, Right fy) -> Right $ \objD -> fx objD && fy objD
            (Left err, _) -> Left err
            (_, Right err) -> Right err
    getMatcher (Not _ exp) =
        (\matcher -> not . matcher) <$> getMatcher exp
    getMatcher (AncestorNameIs _ exp) =
        ancestorNameMatchesWith (==) exp
    getMatcher (AncestorNameStartsWith _ exp) =
        ancestorNameMatchesWith BS.isPrefixOf exp
    getMatcher (AncestorNameEndsWith _ exp) =
        ancestorNameMatchesWith BS.isSuffixOf exp
    getMatcher (AncestorNameContains _ exp) =
        ancestorNameMatchesWith isInfixOf' exp
        where isInfixOf' subStr str = isInfixOf (BS.toStrict subStr) (BS.toStrict str)
    getMatcher (NameIs _ exp) =
        nameMatchesWith (==) exp
    getMatcher (NameStartsWith _ exp) =
        nameMatchesWith BS.isPrefixOf exp
    getMatcher (NameEndsWith _ exp) =
        nameMatchesWith BS.isSuffixOf exp
    getMatcher (NameContains _ exp) =
        nameMatchesWith isInfixOf' exp
        where isInfixOf' subStr str = isInfixOf (BS.toStrict subStr) (BS.toStrict str)
    getMatcher (All _) = Right (\_ -> True)
    getMatcher (None _) = Right (\_ -> False)
    getMatcher (EVar _ _) =
        error "EVar encountered in getMatcher processing; please run deName first"
    getMatcher (EPar _ x) = getMatcher x
    getMatcher (Let _ _ _) =
        error "Let encountered in getMatcher processing; please run deName first"
    getMatcher exp = Left $ Couldn'tParse $ show exp

        -- if innerName == outerName
        --     then getMatcher namedExpr
        --     else Left $ UnrecognizedName (show dec) (show outerName)

matchersToMatcherWithAny :: (Exp a -> MatcherE a) -> [Exp a] -> MatcherE a
matchersToMatcherWithAny f exps =
    let matcherEList = f <$> exps
        eitherListMatcher = sequenceA matcherEList
        matchersToMatcherF = \matchers -> (\objD -> any (\f -> f objD) matchers)
    in matchersToMatcherF <$> eitherListMatcher

ancestorNameMatchesWith :: Show a => NameMatcherFunc -> Exp a -> MatcherE a
ancestorNameMatchesWith f (EString _ x) =
    Right $ \objD -> any (f x) (parents objD)
ancestorNameMatchesWith f (EList _ exps) =
    matchersToMatcherWithAny (ancestorNameMatchesWith f) exps
ancestorNameMatchesWith f exp = Left $ NameMatcherNeedsString $ show exp

nameMatchesWith :: Show a => NameMatcherFunc -> Exp a -> MatcherE a
nameMatchesWith f (EString _ x) = Right $ \objD -> f x $ (BS.pack $ basename objD)
nameMatchesWith f (EList _ exps) = matchersToMatcherWithAny (nameMatchesWith f) exps
nameMatchesWith f exp = Left $ NameMatcherNeedsString $ show exp

