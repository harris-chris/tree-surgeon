module AST
  (
    Exp(..)
    , FsObjData(..)
    , IsMatcher(..)
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
    | IsChildOfNeedsString String
    | CanOnlyFilterDirectory String
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
    show (IsChildOfNeedsString exp) =
        "Error:\n" <> (show exp) <>
        "\nisChildOf must be passed string or list of strings"
    show (CanOnlyFilterDirectory fpath) =
        "Error: unable to filter " <> (show fpath) <>
        "; it is a file, not a directory"

data FsObjData =
    FileData { parents :: [ByteString] }
    deriving (Eq, Show)

class Show a => IsMatcher a where
    getMatcher :: a -> MatcherE a

data Exp a =
    IsChildOf a (Exp a)
    | NameEndsWith a (Exp a)
    | NameContains a (Exp a)
    | Or a (Exp a) (Exp a)
    | And a (Exp a) (Exp a)
    | EPar a (Exp a)
    | EString a ByteString
    | EList a [Exp a]
    deriving (Foldable, Show)

instance Show a => IsMatcher (Exp a) where
    getMatcher (IsChildOf _ exp) = isChildOf exp
    getMatcher (NameEndsWith _ exp) =
        nameMatchesWith BS.isSuffixOf exp
    getMatcher (NameContains _ exp) =
        nameMatchesWith isInfixOf' exp
        where isInfixOf' subStr str = isInfixOf (BS.toStrict subStr) (BS.toStrict str)
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
    getMatcher (EPar _ x) = getMatcher x
    getMatcher exp = Left $ Couldn'tParse $ show exp

matchersToMatcherWithAny :: (Exp a -> MatcherE a) -> [Exp a] -> MatcherE a
matchersToMatcherWithAny f exps =
    let matcherEList = f <$> exps
        eitherListMatcher = sequenceA matcherEList
        matchersToMatcherF = \matchers -> (\n d -> any (\f -> f n d) matchers)
    in matchersToMatcherF <$> eitherListMatcher

isChildOf :: Show a => Exp a -> Either TreeSurgeonException Matcher
isChildOf (EString _ x) = Right $ \_ objData -> elem x $ parents objData
isChildOf (EList _ exps) = matchersToMatcherWithAny isChildOf exps
isChildOf exp = Left $ IsChildOfNeedsString $ show exp

nameMatchesWith :: Show a => NameMatcherFunc -> Exp a -> MatcherE a
nameMatchesWith f (EString _ x) = Right $ \name _ -> f x $ BS.pack name
nameMatchesWith f (EList _ exps) = matchersToMatcherWithAny (nameMatchesWith f) exps
nameMatchesWith f exp = Left $ NameMatcherNeedsString $ show exp

