module Main where

import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory.Tree
import System.IO

import Cli
import Lexer
import Parser
import TreeFilter
import Output

main :: IO ()
main = tsFilter =<< execParser opts
  where
    opts = info (cliOptsParser <**> helper)
      ( fullDesc
     <> progDesc "Filter the directory TARGET using the filter FILTER"
     <> header "Tree surgeon: A utility for filtering and manipulating file trees" )

applyFuncWithFile :: FilePath -> (BS.ByteString -> IO ()) -> IO()
applyFuncWithFile filePath filterFunc =
    withFile filePath ReadMode handleF
        where handleF handle = filterFunc =<< (BS.hGetContents handle)

tsFilter :: CliOptions -> IO ()
tsFilter (ShowFilteredTree fltr source) =
    let applyFilterF = putStrLn . showTree
        f = applyFilterWith source applyFilterF Include
    in case fltr of
        Left str -> f (BS.pack str)
        Right file -> applyFuncWithFile file f
tsFilter (ShowDiffTree fltr source) =
    let applyFilterF = \x y -> putStrLn $ showTreeComparison x y
        f = applyFilterWithComparative source applyFilterF
    in case fltr of
        Left str -> f (BS.pack str)
        Right file -> applyFuncWithFile file f
tsFilter (ToBashArray fltr source includedNotExcluded) =
    let applyFilterF = putStrLn . toBashArray
        f = applyFilterWith source applyFilterF includedNotExcluded
    in case fltr of
        Left str -> f (BS.pack str)
        Right file -> applyFuncWithFile file f
-- tsFilter (WriteFilteredTree filterStr source destination) =
    -- let f = \t -> writeDirectory $ destination :/ t
    -- in applyFilterWith source (BS.pack filterStr) f
-- tsFilter (CliOptions target filterStr (ToBashArray Include)) =
--     let f = putStrLn . toBashArray
--     in applyFilterWith target (BS.pack filterStr) f
-- tsFilter (CliOptions target filterStr (ToBashArray Exclude)) =
--     putStrLn "Bash array exclude"

