module TreeFilter
  (
    filterDir
    , getTree
  ) where

import System.FilePath
import System.Directory.Tree
import AST

toElements :: AnchoredDirTree a -> DirTree [FileName]
toElements (b :/ t) = toElems b [] t
    where toElems p parents (File n _) = File n parents
          toElems p parents (Dir n cs) = Dir n $ map (toElems (p</>n) (n:parents)) cs
          toElems _ _ (Failed n e) = Failed n e

getTree :: FileName -> IO (DirTree [FileName])
getTree fname =
    let
        tree = readDirectoryWith return fname
    in toElements <$> tree

