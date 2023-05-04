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

-- filterTreeFiles :: Exp a -> DirTree FsObjData -> DirTree FsObjData
-- filterTreeFiles exp tree = filterDir filterF tree
--     where filterF (File name objData) = filterObjData exp name objData
--           filterF (Dir _ []) = False
--           filterF (Dir _ (x:xs)) = True
--           filterF (Failed _ _) = True

-- filterTreeDirs :: DirTree FsObjData -> DirTree FsObjData
-- filterTreeDirs tree = filterDir filterF tree
--     where filterF (File _ _) = True
--           filterF (Dir n []) = False
--           filterF (Dir n (x:xs)) = True
--           filterF (Failed _ _) = True

filterTreeF :: Exp a -> DirTree FsObjData -> Bool
filterTreeF exp (File name objData) = filterObjData exp name objData
filterTreeF exp (Dir _ []) = False
filterTreeF exp (Dir name (c:cx)) =
    let contents' = map (filterTreeF exp) (c:cx)
    in not $ null contents'
filterTreeF exp (Failed _ _) = True

filterTreeWith :: Exp a -> DirTree FsObjData -> DirTree FsObjData
filterTreeWith exp tree = filterDir (filterTreeF exp) tree

