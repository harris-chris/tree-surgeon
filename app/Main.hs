module Main where

import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory.Tree

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

tsFilter :: CliOptions -> IO ()
tsFilter (ShowFilteredTree filterStr source) =
    let f = putStrLn . showTree
    in applyFilterWith source (BS.pack filterStr) f
tsFilter (ShowDiffTree filterStr source) =
    let f = \x y -> putStrLn $ showTreeComparison x y
    in applyFilterWithComparative source (BS.pack filterStr) f
tsFilter (ToBashArray filterStr source) =
    let f = putStrLn . toBashArray
    in applyFilterWith source (BS.pack filterStr) f
tsFilter (WriteFilteredTree filterStr source destination) =
    let f = \t -> writeDirectory $ destination :/ t
    in applyFilterWith source (BS.pack filterStr) f
-- tsFilter (CliOptions target filterStr (ToBashArray Include)) =
--     let f = putStrLn . toBashArray
--     in applyFilterWith target (BS.pack filterStr) f
-- tsFilter (CliOptions target filterStr (ToBashArray Exclude)) =
--     putStrLn "Bash array exclude"

