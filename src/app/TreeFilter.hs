module TreeFilter
  (
    applyFilterWith
    , applyFilterWithComparative
    , filterDir
    , filterTreeWith
    , getExcluded
    , getFDataTree
    , toFData
  ) where

import Control.Arrow (left)
import Control.Exception
-- import Control.Monad.Logger.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either (partitionEithers)
import Data.Maybe
import Debug.Trace (trace)
import qualified Data.Text as T
import System.Directory.Tree
import System.Exit
import TextShow
import qualified Lexer as X

import Cli
import FData
import Output
import Exceptions
import ExceptionFuncs
import Functions
import IsTrace hiding (trace)
import Parser (parseTreeSurgeon)
import SimpleExp
import SimpleExpFuncs
import ResolvedFuncs
import TextShow

type Matcher a = FData -> EitherF a Bool

getFDataTree :: FileName -> IO (Either [TSException X.Range] (DirTree FData))
getFDataTree dirName = do
    (_ :/ plainTree) <- readDirectoryWith return dirName
    let fDataTree = toFData True [] plainTree
    return $ validateTree dirName fDataTree

toFData :: Bool -> [T.Text] -> DirTree a -> DirTree FData
toFData _ parents (File name _) =
    File name (FData (T.pack name) (reverse parents) FileFileType)
-- Exclude the source directory from parents, because it's pointless to include it
toFData True parents (Dir name contents) =
    let contents' = map (toFData False parents) contents
    in Dir name contents'
toFData False parents (Dir name contents) =
    let contents' = map (toFData False ((T.pack name):parents)) contents
    in Dir name contents'
toFData _ _ (Failed name err) = Failed name err

applyFilterWith :: FileName -> (DirTree FData -> IO()) -> BS.ByteString -> IO ()
applyFilterWith dirName ioF filterStr = do
    origTreeE <- getFDataTree dirName
    let filteredTreeE = filterTreeWith filterStr =<< origTreeE
    case filteredTreeE of
        Left (e:_) -> die $ toErrorMessage filterStr e
        Right filtered -> ioF filtered
        _ -> error "Wie ist es moglich"

applyFilterWithComparative :: FileName -> (DirTree FData -> DirTree FData -> IO()) -> BS.ByteString -> IO ()
applyFilterWithComparative dirName ioF filterStr = do
    origTreeE <- getFDataTree dirName
    let filteredTreeE = filterTreeWith filterStr =<< origTreeE
    case (origTreeE, filteredTreeE) of
        (Left (e:_), _) -> die $ toErrorMessage filterStr e
        (_, Left (e:_)) -> die $ toErrorMessage filterStr e
        (Right orig, Right filtered) -> ioF filtered orig
        _ -> error "Wie ist es moglich"

getExcluded :: Maybe TypeConstraint -> DirTree FData -> DirTree FData -> [String]
getExcluded typeConstraint origTree filteredTree =
    let arrayOrig = toBashArray typeConstraint origTree
        arrayFiltered = toBashArray typeConstraint filteredTree
    in filter (\z -> not $ elem z arrayFiltered) arrayOrig

filterTreeWith :: BS.ByteString -> DirTree FData -> Either [TSException X.Range] (DirTree FData)
filterTreeWith filterStr validatedTree = do
    simpleExp <- filterStrToSimpleExp filterStr
    let matcher = applyExpression simpleExp
    filtered <- filterTreeWith' [] matcher validatedTree
    return $ fromJust filtered

applyExpression :: IsTrace trc => SimpleExp trc -> FData -> EitherF trc Bool
applyExpression expr fData = do
    resolved <- simpleExpToResolved functions fData expr
    asBool <- convertToBool resolved
    return asBool

validateTree :: FileName -> DirTree a -> Either [TSException X.Range] (DirTree a)
validateTree dirName (File _ _) = Left $ [Other $ CanOnlyFilterDirectory $ T.pack dirName]
validateTree dirName tree =
    case failures tree of
        [] -> Right tree
        xs -> Left $ (\fl -> Other $ FailedToReadTree (T.pack $ dirName)) <$> xs

filterTreeWith' ::
    [T.Text] -> Matcher a -> DirTree FData -> Either [TSException a] (Maybe (DirTree FData))
filterTreeWith' parents f (Dir name contents) =
    -- The very first parent will be the source directory; we want to exclude this
    let parentsWithoutSourceDir = if length parents == 0
                                      then []
                                      else tail $ reverse parents
        fData = FData (T.pack name) parentsWithoutSourceDir DirFileType
        parents' = (T.pack name):parents
        contents' = filterTreeWith' parents' f <$> contents
        (exceptions, contents'') = partitionEithers contents'
        contents''' = catMaybes contents''
    in
        case f fData of
            Left err -> Left [Filter err]
            Right True ->
                if null exceptions
                then Right $ Just $ Dir name contents'''
                else Left $ concat exceptions
            Right False ->
                -- Some exceptions to the usual logic
                -- if a folder has contents, it is saved
                -- or if it's the top-level folder, it is saved
                if (length contents''' > 0) || (length parents == 0)
                then Right (Just $ Dir name contents''')
                else Right Nothing
filterTreeWith' (_:_) f file@(File _ fData) =
    case f fData of
        Left err -> Left [Filter err]
        Right result ->
            if result
                then Right $ Just file
                else Right Nothing
filterTreeWith' _ _ _ = error "Failed to match within tree; tree should have been validated"

filterStrToSimpleExp :: BS.ByteString -> Either [TSException X.Range] (SimpleExp X.Range)
filterStrToSimpleExp filterStr =
    let parsed = X.runAlex filterStr parseTreeSurgeon
    in case parsed of
        Left _ -> Left [Other Can'tParse]
        Right p -> left (\e -> [Filter e]) $ rawExpToSimpleExp p

