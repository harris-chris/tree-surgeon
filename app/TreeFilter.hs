module TreeFilter
  (
    filterDir
    , applyFilterWith
  ) where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
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
        Left str -> putStrLn str
        Right exp -> (filterTreeWith exp <$> treeIO') >>= ioF

filterTreeFiles :: Exp a -> DirTree FsObjData -> Bool
filterTreeFiles exp (File name objData) = filterObjData exp (pack name) objData
filterTreeFiles exp (Dir _ _) = True
filterTreeFiles exp (Failed _ _) = True

filterTreeDirs :: DirTree FsObjData -> Bool
filterTreeDirs (File _ _) = True
filterTreeDirs (Dir _ []) = False
filterTreeDirs (Dir _ (c:cx)) = True
filterTreeDirs (Failed _ _) = True

filterTreeWith :: Exp a -> DirTree FsObjData -> DirTree FsObjData
filterTreeWith exp tree =
    let filesFiltered = filterDir (filterTreeFiles exp) tree
    in filterDir filterTreeDirs filesFiltered

