module TreeFilter
  (
    filterDir
    , applyFilterWith
    , applyFilterWithComparative
    , TreeSurgeonException(..)
    , InclExcl(..)
  ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Maybe
import Debug.Trace
import System.FilePath
import System.Directory.Tree
import AST
import qualified Lexer as L
import Parser (parseTreeSurgeon)

data InclExcl = Include | Exclude
    deriving Show

toElements :: DirTree a -> DirTree FsObjData
toElements t = toElements' [] t

toElements' :: [ByteString] -> DirTree a -> DirTree FsObjData
toElements' parents (File name _) = File name (FileData parents)
toElements' parents (Dir name contents) =
    let contents' = map (toElements' ((pack name):parents)) contents
    in Dir name contents'
toElements' _ (Failed name error) = Failed name error

applyFilterWith :: FileName -> (DirTree FsObjData -> IO()) -> InclExcl -> ByteString -> IO ()
applyFilterWith dirname ioF inclExcl filterStr  =
    do
        anchoredTree <- readDirectoryWith return dirname
        let filteredTreeE = filterTreeWith (dirTree anchoredTree) inclExcl filterStr
        case filteredTreeE of
            Left err -> throw $ err
            Right filtered -> ioF filtered

applyFilterWithComparative :: FileName -> (DirTree FsObjData -> DirTree FsObjData -> IO()) -> ByteString -> IO ()
applyFilterWithComparative dirname ioF filterStr =
    do
        anchoredTree <- readDirectoryWith return dirname
        let filteredTreeE = filterTreeWith (dirTree anchoredTree) Include filterStr
        case filteredTreeE of
            Left err -> throw $ err
            Right filtered -> ioF (toElements $ dirTree anchoredTree) filtered

filterTreeFilesWith :: Matcher -> InclExcl -> DirTree FsObjData -> Bool
filterTreeFilesWith f Include (File name objData) = f name objData
filterTreeFilesWith f Exclude (File name objData) = not $ f name objData
filterTreeFilesWith _ Include _ = True
filterTreeFilesWith _ Exclude _ = False

filterTreeDirs' :: DirTree FsObjData -> Maybe (DirTree FsObjData)
filterTreeDirs' f@(File _ _) = Just f
filterTreeDirs' f@(Failed _ _) = Just f
filterTreeDirs' (Dir name contents) =
    let filtered = filterTreeDirs' <$> contents
        hasContents = any isJust filtered
    in if hasContents
        then Just $ Dir name $ catMaybes filtered
        else Nothing

filterTreeDirs :: InclExcl -> DirTree FsObjData -> Bool
filterTreeDirs _ (File _ _) = True
filterTreeDirs Include (Dir name []) = False
filterTreeDirs Exclude (Dir name []) = True
filterTreeDirs Include (Dir _ (c:cx)) = True
filterTreeDirs Exclude (Dir _ (c:cx)) = False
filterTreeDirs _ (Failed _ _) = True

filterTreeWith :: DirTree a -> InclExcl -> ByteString -> Either TreeSurgeonException (DirTree FsObjData)
filterTreeWith tree inclExcl filterStr =
    let expE = L.runAlex filterStr parseTreeSurgeon
        expE' = (case expE of
            Left errStr -> Left $ Couldn'tLex errStr
            Right exprString -> Right exprString) :: Either TreeSurgeonException (Exp L.Range)
        matcherE = expE' >>= getMatcher
    in matcherE >>= filterTreeWith' (toElements tree) inclExcl

filterTreeWith' :: DirTree FsObjData -> InclExcl -> Matcher -> Either TreeSurgeonException (DirTree FsObjData)
filterTreeWith' tree@(Dir name _) inclExcl fileMatcher =
    let filesFiltered = filterDir (filterTreeFilesWith fileMatcher inclExcl) tree
        filteredContents = filterTreeDirs' <$> (contents filesFiltered)
    in Right $ Dir name (catMaybes filteredContents)
filterTreeWith' (File name _) _ _ = Left $ CanOnlyFilterDirectory name
filterTreeWith' (Failed name _) _ _ = Left $ CanOnlyFilterDirectory name
