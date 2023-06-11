module AST
  (
    Exp(..)
    , Dec(..)
    , Name(..)
    , FsObjData(..)
    , IsMatcher(..)
    , IsFilePath(..)
    , TreeSurgeonException(..)
    , Matcher
  ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Char8 (isInfixOf)
import Debug.Trace
import System.FilePath
import System.Directory.Tree

type Matcher = (FileName -> FsObjData -> Bool)
type MatcherE a = Either TreeSurgeonException Matcher
type NameMatcherFunc = (ByteString -> ByteString -> Bool)

data TreeSurgeonException
    = Couldn'tParse String
    | Couldn'tLex String
    | NameMatcherNeedsString String
    | AncestorNameIsNeedsString String
    | CanOnlyFilterDirectory String
    | UnrecognizedName String String
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
    show (UnrecognizedName dec name) =
        "Error: Unrecognized name " <> name
        <> " in declaration " <> dec

data FsObjData =
    FileData { parents :: [ByteString] }
    deriving (Eq, Show, Ord)

class IsFilePath a where
    toFilePath :: a -> FilePath

instance IsFilePath FsObjData where
    toFilePath (FileData pts) = joinPath $ BS.unpack <$> pts

class Show a => IsMatcher a where
    getMatcher :: a -> MatcherE a

data Name a
    = Name a ByteString
    deriving (Foldable, Show)

instance Eq (Name a) where
    (==) (Name _ x) (Name _ y) = x == y

data Dec a
  = Let a (Name a) (Exp a) (Exp a)
  deriving (Foldable, Show)

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
    | EVar a (Name a)
    | EPar a (Exp a)
    | EString a ByteString
    | EList a [Exp a]
    | Declaration (Dec a)
    deriving (Foldable, Show)

instance Show a => IsMatcher (Exp a) where
    getMatcher (Or _ x y) =
        case ((getMatcher x), (getMatcher y)) of
            (Right fx, Right fy) -> Right $ \n d -> fx n d || fy n d
            (Left err, _) -> Left err
            (_, Right err) -> Right err
    getMatcher (And _ x y) =
        case ((getMatcher x), (getMatcher y)) of
            (Right fx, Right fy) -> Right $ \n d -> fx n d && fy n d
            (Left err, _) -> Left err
            (_, Right err) -> Right err
    getMatcher (Not _ exp) =
        let invertMatcher = \matcher -> (\fn fsObj -> not $ matcher fn fsObj)
        in invertMatcher <$> getMatcher exp
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
    getMatcher (All _) = Right (\_ _ -> True)
    getMatcher (None _) = Right (\_ _ -> False)
    getMatcher (EPar _ x) = getMatcher x
    getMatcher (Declaration _) =
        error "Let encountered in getMatcher processing; please run deName first"
    getMatcher exp = Left $ Couldn'tParse $ show exp

        -- if innerName == outerName
        --     then getMatcher namedExpr
        --     else Left $ UnrecognizedName (show dec) (show outerName)

matchersToMatcherWithAny :: (Exp a -> MatcherE a) -> [Exp a] -> MatcherE a
matchersToMatcherWithAny f exps =
    let matcherEList = f <$> exps
        eitherListMatcher = sequenceA matcherEList
        matchersToMatcherF = \matchers -> (\n d -> any (\f -> f n d) matchers)
    in matchersToMatcherF <$> eitherListMatcher

ancestorNameMatchesWith :: Show a => NameMatcherFunc -> Exp a -> MatcherE a
ancestorNameMatchesWith f (EString _ x) =
    Right $ \name fsObj -> any (f x) (parents fsObj)
ancestorNameMatchesWith f (EList _ exps) =
    matchersToMatcherWithAny (ancestorNameMatchesWith f) exps
ancestorNameMatchesWith f exp = Left $ NameMatcherNeedsString $ show exp

nameMatchesWith :: Show a => NameMatcherFunc -> Exp a -> MatcherE a
nameMatchesWith f (EString _ x) = Right $ \name _ -> f x $ BS.pack name
nameMatchesWith f (EList _ exps) = matchersToMatcherWithAny (nameMatchesWith f) exps
nameMatchesWith f exp = Left $ NameMatcherNeedsString $ show exp

