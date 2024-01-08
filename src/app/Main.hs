module Main where

import Control.Monad (void)
import Main.Utf8
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.IO
import System.Directory (makeAbsolute)
import System.Directory.Tree (writeDirectoryWith, AnchoredDirTree(..), DirTree)
import System.FilePath (joinPath)

import Cli
import FData
import Output
import ToGitExclude
import TreeFilter

import Debug.Trace

main :: IO ()
main = withUtf8 $ tsFilter =<< execParser opts
    where
        header' = "Tree surgeon: A utility for filtering and manipulating file trees"
        progDesc' =
            "Use --help to see a list of commands, or "
            <> " COMMAND --help on a specific command to see more information; eg"
            <> " tree-surgeon tree --help"
            <> " to see help information on the tree command"
        fullDesc' = fullDesc <> header header' <> progDesc progDesc'
        opts = info
            (cliOptsParser <**> helper) fullDesc'

applyFuncWithFile :: FilePath -> (LBS.ByteString -> IO ()) -> IO()
applyFuncWithFile filePath filterFunc =
    withFile filePath ReadMode handleF
        where handleF handle = filterFunc =<< (LBS.hGetContents handle)

adjustSource :: PathsType -> FilePath -> IO FilePath
adjustSource RelToSource source = pure "."
adjustSource Absolute source = makeAbsolute source

toBashArrayIncludeF :: FilePath -> Maybe TypeConstraint -> PathsType -> DirTree a -> IO ()
toBashArrayIncludeF source typeConstraint pathsType tree = do
    source' <- adjustSource pathsType source
    let tree' = applyPaths source' tree
    let asBashArray = toBashArray typeConstraint tree'
    let asStr = unwords asBashArray
    putStrLn asStr

toBashArrayExcludeF ::
    FilePath -> Maybe TypeConstraint -> PathsType -> DirTree FData -> DirTree FData -> IO ()
toBashArrayExcludeF source typeConstraint pathsType filtered orig = do
    source' <- adjustSource pathsType source
    let orig' = applyPaths source' orig
    let filtered' = applyPaths source' filtered
    let asBashArray = getExcluded typeConstraint orig' filtered'
    let asStr = unwords asBashArray
    putStrLn asStr

tsFilter :: CliOptions -> IO ()
tsFilter (ShowFilteredTree fltr source) =
    let applyFilterF = putStrLn . showTree
        f = applyFilterWith source applyFilterF
    in case fltr of
        Left filterStr -> f (LBS.pack filterStr)
        Right file -> applyFuncWithFile file f
tsFilter (ShowDiffTree fltr source) =
    let applyFilterF = \filtered orig -> putStrLn $ showTreeComparison orig filtered
        f = applyFilterWithComparative source applyFilterF
    in case fltr of
        Left filterStr -> f (LBS.pack filterStr)
        Right file -> applyFuncWithFile file f
tsFilter (ToBashArray fltr source False typeConstraint pathsType) =
    let applyFilterF = toBashArrayIncludeF source typeConstraint pathsType
        f = applyFilterWith source applyFilterF
    in case fltr of
        Left filterStr -> f (LBS.pack filterStr)
        Right file -> applyFuncWithFile file f
tsFilter (ToBashArray fltr source True typeConstraint pathsType) =
    let applyFilterF = toBashArrayExcludeF source typeConstraint pathsType
        f = applyFilterWithComparative source applyFilterF
    in case fltr of
        Left filterStr -> f (LBS.pack filterStr)
        Right file -> applyFuncWithFile file f
tsFilter (ToGitExclude fltr) =
    let resolved = simpleExpToResolved functions
    let f = putStrLn $ toGitExclude fltr
    in case fltr of
        Left filterStr -> f (LBS.pack filterStr)
        Right file -> applyFuncWithFile file f
