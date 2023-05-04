module TreeFilter
  (
    filterDir
    , applyFilterWith
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import System.FilePath
import System.Directory.Tree
import AST
import qualified Lexer as L
import Parser (parseTreeSurgeon)

toElements :: AnchoredDirTree a -> DirTree FsObjData
toElements (b :/ t) = toElements' [] t

toElements' :: [FileName] -> DirTree a -> DirTree FsObjData
toElements' parents (File name _) = File name (FileData parents)
toElements' parents (Dir name contents) =
    let contents' = map (toElements' (name:parents)) contents
    in Dir name contents'
toElements' _ (Failed name error) = Failed name error

applyFilterWith :: FileName -> ByteString -> (DirTree FsObjData -> IO()) -> IO ()
applyFilterWith dirname filterStr ioF =
    let treeIO = readDirectoryWith return dirname
        treeIO' = toElements <$> treeIO
    in case L.runAlex filterStr parseTreeSurgeon of
        Left str -> putStrLn str
        Right exp -> (filterTree exp <$> treeIO') >>= ioF

filterTree :: Exp a -> DirTree FsObjData -> DirTree FsObjData
filterTree exp tree = filterDir filterF tree
    where filterF (File name objData) = filterObjData exp name objData
          filterF (Dir _ _) = True
          filterF (Failed _ _) = True

