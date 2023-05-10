module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Options.Applicative
import Data.Semigroup ((<>))

import Lexer
import Parser
import TreeFilter
import TreeShow

main :: IO ()
main = tsFilter =<< execParser opts
  where
    opts = info (cliOptsParser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

tsFilter :: CliOptions -> IO ()
tsFilter (CliOptions target filterStr (ShowTree True)) =
    let f = \x y -> putStrLn $ showTreeComparison x y
    in applyFilterWithComparative target (BS.pack filterStr) f
tsFilter (CliOptions target filterStr (ShowTree False)) =
    let f = putStrLn . showTree
    in applyFilterWith target (BS.pack filterStr) f
tsFilter (CliOptions target filterStr (ToBashArray Include)) =
    putStrLn "Bash array include"
tsFilter (CliOptions target filterStr (ToBashArray Exclude)) =
    putStrLn "Bash array exclude"

showTreeTypeParser :: Parser Bool
showTreeTypeParser = switch (
    long "diff" <> help "Show diff between original tree and filtered tree"
    )

data IncludeExclude = Include | Exclude

inclExclParser :: Parser IncludeExclude
inclExclParser = includeParser <|> excludeParser
     where includeParser = flag' Include (long "include")
           excludeParser = flag' Exclude (long "exclude")

data CliOptions = CliOptions
    { targetDir :: String
    , filterStr :: String
    , optCommand :: Command }

data Command
    = ShowTree Bool
    | ToBashArray IncludeExclude

showTreeParser :: Parser Command
showTreeParser = ShowTree <$> showTreeTypeParser

toBashArrayParser :: Parser Command
toBashArrayParser = ToBashArray <$> inclExclParser

commandSubParser = subparser
    ( command "show-tree" (
        info showTreeParser ( progDesc "Add a file to the repository" ))
    <> command "to-bash-array" (info toBashArrayParser ( progDesc "Record changes to the repository" ))
    <> command "to-bash-array" (info toBashArrayParser ( progDesc "Record changes to the repository" ))
    )

cliOptsParser :: Parser CliOptions
cliOptsParser = CliOptions
    <$> argument str
        ( metavar "TARGET"
        <> help "The directory on which to filter" )
    <*> argument str
        ( metavar "FILTER"
        <> help "The filter string to apply" )
    <*> commandSubParser



-- data ShowTreeOpts =
--     ShowTreeOpts { stTarget :: String , stFilterText :: String, treeType :: ShowTreeType }

-- showTreeOptsParser :: Parser ShowTreeOpts
-- showTreeOptsParser =
--     ShowTreeOpts <$> argument str
--         ( metavar "TARGET"
--         <> help "Target directory" )
--     <*> argument str
--         ( metavar "FILTER"
--         <> help "The filter to apply" )
--     <*> showTreeTypeParser

-- data ToBashArrayOpts =
--     ToBashArrayOpts { baTarget :: String , baFilterText :: String, inclExcl :: IncludeExclude }

-- toBashArrayOptsParser :: Parser ToBashArrayOpts
-- toBashArrayOptsParser =
--     ToBashArrayOpts <$> argument str
--         ( metavar "TARGET"
--         <> help "Target directory" )
--     <*> argument str
--         ( metavar "FILTER"
--         <> help "The filter to apply" )
--     <*> inclExclParser

-- toBashArrayParser =
--     argument str
--         ( metavar "TARGET"
--         <> help "Target directory" )
--     <*> argument str
--         ( metavar "FILTER"
--         <> help "The filter to apply" )
--     <*> inclExclParser

-- basicParser :: Parser String
-- basicParser =
--     argument str
--         ( metavar "TARGET"
--         <> help "Target directory" )
--     <*> argument str
--         ( metavar "FILTER"
--         <> help "The filter to apply" )

-- showTreeParser ::
-- showTreeParser =
--     argument str
--         ( metavar "TARGET"
--         <> help "Target directory" )
--     <*> argument str
--         ( metavar "FILTER"
--         <> help "The filter to apply" )
--     <*> showTreeTypeParser

-- toBashArrayParser =
--     argument str
--         ( metavar "TARGET"
--         <> help "Target directory" )
--     <*> argument str
--         ( metavar "FILTER"
--         <> help "The filter to apply" )
--     <*> inclExclParser

      -- <*> option auto
      --     ( long "enthusiasm"
      --    <> help "How enthusiastically to greet"
      --    <> showDefault
      --    <> value 1
      --    <> metavar "INT" )

