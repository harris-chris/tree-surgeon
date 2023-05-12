module Main where

import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS

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
tsFilter (CliOptions target filterStr (ShowTree True)) =
    let f = \x y -> putStrLn $ showTreeComparison x y
    in applyFilterWithComparative target (BS.pack filterStr) f
tsFilter (CliOptions target filterStr (ShowTree False)) =
    let f = putStrLn . showTree
    in applyFilterWith target (BS.pack filterStr) f
tsFilter (CliOptions target filterStr (ToBashArray Include)) =
    let f = putStrLn . toBashArray
    in applyFilterWith target (BS.pack filterStr) f
tsFilter (CliOptions target filterStr (ToBashArray Exclude)) =
    putStrLn "Bash array exclude"

