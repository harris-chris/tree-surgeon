module TreeFilter
  (
    applyFilterWith
    , applyFilterWithComparative
    , filterDir
    , filterTreeWith
    , getExcluded
    , parseFilterStr
    , toElements
    , TSException(..)
  ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Either (partitionEithers)
import Data.Maybe
import Debug.Trace
import System.FilePath
import System.Directory.Tree
import qualified Lexer as L

import AST
import ASTFuncs
import Output
import TSException
import Parser (parseTreeSurgeon)

toElements :: DirTree a -> DirTree FData
toElements t = toElements' [] t

toElements' :: [ByteString] -> DirTree a -> DirTree FData
toElements' parents (File name _) = File name (FileData name parents)
toElements' parents (Dir name contents) =
    let contents' = map (toElements' ((pack name):parents)) contents
    in Dir name contents'
toElements' _ (Failed name error) = Failed name error

applyFilterWith :: FileName -> (DirTree FData -> IO()) -> ByteString -> IO ()
applyFilterWith dirname ioF filterStr  =
    do
        anchoredTree <- readDirectoryWith return dirname
        let filteredTreeE = filterTreeWith (dirTree anchoredTree) filterStr
        case filteredTreeE of
            Left (e:es) -> throw $ e
            Right filtered -> ioF filtered

applyFilterWithComparative :: FileName -> (DirTree FData -> DirTree FData -> IO()) -> ByteString -> IO ()
applyFilterWithComparative dirname ioF filterStr =
    do
        anchoredTree <- readDirectoryWith return dirname
        let filteredTreeE = filterTreeWith (dirTree anchoredTree) filterStr
        case filteredTreeE of
            Left (e:es) -> throw $ e
            Right filtered -> ioF (toElements $ dirTree anchoredTree) filtered

getExcluded :: Bool -> DirTree FData -> DirTree FData -> [String]
getExcluded ancestors origTree filteredTree =
    let arrayOrig = toBashArray ancestors origTree
        arrayFiltered = toBashArray ancestors filteredTree
    in filter (\z -> not $ elem z arrayFiltered) arrayOrig

-- filterTreeFilesWith :: (FData -> Either TSException Bool) -> DirTree FData -> Bool
-- filterTreeFilesWith f (File name objData) = f objData
-- filterTreeFilesWith _ _ = True

filterTreeDirs' :: DirTree FData -> Maybe (DirTree FData)
filterTreeDirs' f@(File _ _) = Just f
filterTreeDirs' f@(Failed _ _) = Just f
filterTreeDirs' (Dir name contents) =
    let filtered = filterTreeDirs' <$> contents
        hasContents = any isJust filtered
    in if hasContents
        then Just $ Dir name $ catMaybes filtered
        else Nothing

filterTreeDirs :: DirTree FData -> Bool
filterTreeDirs (File _ _) = True
filterTreeDirs (Dir name []) = False
filterTreeDirs (Dir _ (c:cx)) = True
filterTreeDirs (Failed _ _) = True

filterTreeWith :: DirTree a -> ByteString -> Either [TSException] (DirTree FData)
filterTreeWith tree filterStr =
    let exp = parseFilterStr filterStr
        simplifiedExp = simplifyExp =<< exp
        tree' = toElements tree
    in case simplifiedExp of
        Left err -> Left [err]
        Right simplified ->
            let filtered = filterTreeWith' True (getMatcher simplified) tree'
            in fromJust <$> filtered

filterTreeWith' :: Bool -> Matcher -> DirTree FData -> Either [TSException] (Maybe (DirTree FData))
filterTreeWith' isBase f (Dir name contents) =
    let contents' = filterTreeWith' False f <$> contents :: [Either [TSException] (Maybe (DirTree FData))]
        (exceptions, contents'') = partitionEithers contents'
        contents''' = catMaybes contents''
        -- We have a situation here: Dir has no `FData` so can't match on that
        -- Perhaps construct an FData here?
        -- includeMe = f contents
    in if null exceptions
        then if length contents''' == 0
                 -- if a folder has no contents, we get rid of it by default
                 then Right Nothing
                 else Right (Just $ Dir name contents''')
        else Left $ concat exceptions
filterTreeWith' True _ (File name _) = Left [CanOnlyFilterDirectory name]
filterTreeWith' False f file@(File name fData) =
    case f fData of
        Left err -> Left [err]
        Right result ->
            if result
                then Right $ Just file
                else Right Nothing
filterTreeWith' _ _ (Failed name _) = Left [CanOnlyFilterDirectory name]

parseFilterStr :: ByteString -> Either TSException (Exp L.Range)
parseFilterStr filterStr =
    let parsed = L.runAlex filterStr parseTreeSurgeon
    in case parsed of
        Left errStr -> Left $ Couldn'tLex errStr
        Right exprString -> Right exprString

