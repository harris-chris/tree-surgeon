module TreeFilter
  (
    filterDir
    , applyFilterWith
    , TreeSurgeonException(..)
  ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Debug.Trace
import System.FilePath
import System.Directory.Tree
import AST
import qualified Lexer as L
import Parser (parseTreeSurgeon)

toElements :: AnchoredDirTree a -> DirTree FsObjData
toElements (b :/ t) = toElements' [] t

toElements' :: [ByteString] -> DirTree a -> DirTree FsObjData
toElements' parents (File name _) = File name (FileData parents)
toElements' parents (Dir name contents) =
    let contents' = map (toElements' ((pack name):parents)) contents
    in Dir name contents'
toElements' _ (Failed name error) = Failed name error

applyFilterWith :: FileName -> ByteString -> (DirTree FsObjData -> IO()) -> IO ()
applyFilterWith dirname filterStr ioF =
    let treeIO = readDirectoryWith return dirname
        treeIO' = toElements <$> treeIO
    in case L.runAlex filterStr parseTreeSurgeon of
        Left errMsg -> throw $ Couldn'tParseExp (unpack filterStr) errMsg
        Right exp -> (filterTreeWith exp <$> treeIO') >>= ioF

filterTreeFiles :: Show a => Exp a -> DirTree FsObjData -> Either TreeSurgeonException Bool
filterTreeFiles exp (File name objData) =
    case filterObjData exp (pack name) objData of
        Left err -> err
        Right
filterTreeFiles exp (Dir _ _) = Right True
filterTreeFiles exp (Failed _ _) = Right True

filterTreeDirs :: DirTree FsObjData -> Bool
filterTreeDirs (File _ _) = True
filterTreeDirs (Dir _ []) = False
filterTreeDirs (Dir _ (c:cx)) = True
filterTreeDirs (Failed _ _) = True

filterTreeWith :: Show a => Exp a -> DirTree FsObjData -> Either TreeSurgeonException (DirTree FsObjData)
filterTreeWith exp tree =
    let filesFilteredTree = filterDir (filterTreeFiles exp) tree
    in filterDir filterTreeDirs filesFilteredTree

