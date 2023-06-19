module TreeFilter
  (
    applyFilterWith
    , applyFilterWithComparative
    , filterDir
    , filterTreeWith
    , getExcluded
    , toElements
    , TreeSurgeonException(..)
  ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Maybe
import Debug.Trace
import System.FilePath
import System.Directory.Tree
import AST
import qualified Lexer as L
import Output
import Parser (parseTreeSurgeon)

toElements :: DirTree a -> DirTree FsObjData
toElements t = toElements' [] t

toElements' :: [ByteString] -> DirTree a -> DirTree FsObjData
toElements' parents (File name _) = File name (FileData name parents)
toElements' parents (Dir name contents) =
    let contents' = map (toElements' ((pack name):parents)) contents
    in Dir name contents'
toElements' _ (Failed name error) = Failed name error

applyFilterWith :: FileName -> (DirTree FsObjData -> IO()) -> ByteString -> IO ()
applyFilterWith dirname ioF filterStr  =
    do
        anchoredTree <- readDirectoryWith return dirname
        let filteredTreeE = filterTreeWith (dirTree anchoredTree) filterStr
        case filteredTreeE of
            Left err -> throw $ err
            Right filtered -> ioF filtered

applyFilterWithComparative :: FileName -> (DirTree FsObjData -> DirTree FsObjData -> IO()) -> ByteString -> IO ()
applyFilterWithComparative dirname ioF filterStr =
    do
        anchoredTree <- readDirectoryWith return dirname
        let filteredTreeE = filterTreeWith (dirTree anchoredTree) filterStr
        case filteredTreeE of
            Left err -> throw $ err
            Right filtered -> ioF (toElements $ dirTree anchoredTree) filtered

getExcluded :: Bool -> DirTree FsObjData -> DirTree FsObjData -> [String]
getExcluded ancestors origTree filteredTree =
    let arrayOrig = toBashArray ancestors origTree
        arrayFiltered = toBashArray ancestors filteredTree
    in filter (\z -> not $ elem z arrayFiltered) arrayOrig

filterTreeFilesWith :: Matcher -> DirTree FsObjData -> Bool
filterTreeFilesWith f (File name objData) = f objData
filterTreeFilesWith _ _ = True

filterTreeDirs' :: DirTree FsObjData -> Maybe (DirTree FsObjData)
filterTreeDirs' f@(File _ _) = Just f
filterTreeDirs' f@(Failed _ _) = Just f
filterTreeDirs' (Dir name contents) =
    let filtered = filterTreeDirs' <$> contents
        hasContents = any isJust filtered
    in if hasContents
        then Just $ Dir name $ catMaybes filtered
        else Nothing

filterTreeDirs :: DirTree FsObjData -> Bool
filterTreeDirs (File _ _) = True
filterTreeDirs (Dir name []) = False
filterTreeDirs (Dir _ (c:cx)) = True
filterTreeDirs (Failed _ _) = True

filterTreeWith :: DirTree a -> ByteString -> Either TreeSurgeonException (DirTree FsObjData)
filterTreeWith tree filterStr =
    let expE = L.runAlex filterStr parseTreeSurgeon
        expE' = (case expE of
            Left errStr -> Left $ Couldn'tLex errStr
            Right exprString -> Right exprString) :: Either TreeSurgeonException (Exp L.Range)
        denamedExpE' = deName =<< expE'
        matcherE = getMatcher =<< denamedExpE'
    in matcherE >>= filterTreeWith' (toElements tree)

filterTreeWith' :: DirTree FsObjData -> Matcher -> Either TreeSurgeonException (DirTree FsObjData)
filterTreeWith' tree@(Dir name _) fileMatcher =
    let filesFiltered = filterDir (filterTreeFilesWith fileMatcher) tree
        filteredContents = filterTreeDirs' <$> (contents filesFiltered)
    in Right $ Dir name (catMaybes filteredContents)
filterTreeWith' (File name _) _ = Left $ CanOnlyFilterDirectory name
filterTreeWith' (Failed name _) _ = Left $ CanOnlyFilterDirectory name
