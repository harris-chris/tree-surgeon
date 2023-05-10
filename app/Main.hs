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
tsFilter (CliOptions (ShowTree Comparative) target filterStr) =
    let f = \x y -> putStrLn $ showTreeComparison x y
    in applyFilterWithComparative target (BS.pack filterStr) f
tsFilter (CliOptions (ShowTree FilteredOnly) target filterStr) =
    let f = putStrLn . showTree
    in applyFilterWith target (BS.pack filterStr) f
tsFilter (CliOptions (ToBashArray Include) target filterStr) =
    putStrLn "Bash array include"
tsFilter (CliOptions (ToBashArray Exclude) target filterStr) =
    putStrLn "Bash array exclude"

data ShowTreeType = Comparative | FilteredOnly

showTreeTypeParser :: Parser ShowTreeType
showTreeTypeParser = comparativeParser <|> filteredOnlyParser
     where comparativeParser = flag' Comparative (long "comparative")
           filteredOnlyParser = flag' FilteredOnly (long "filtered-only")

data IncludeExclude = Include | Exclude

inclExclParser :: Parser IncludeExclude
inclExclParser = includeParser <|> excludeParser
     where includeParser = flag' Include (long "include")
           excludeParser = flag' Exclude (long "exclude")

data CliOptions = CliOptions
    { optCommand :: Command
    , targetDir :: String
    , filterStr :: String }

data Command
    = ShowTree ShowTreeType
    | ToBashArray IncludeExclude

showTreeParser :: Parser Command
showTreeParser = ShowTree <$> showTreeTypeParser

toBashArrayParser :: Parser Command
toBashArrayParser = ToBashArray <$> inclExclParser

commandSubParser = subparser
    ( command "show-tree" (info showTreeParser ( progDesc "Add a file to the repository" ))
   <> command "to-bash-array" (info toBashArrayParser ( progDesc "Record changes to the repository" ))
    )

cliOptsParser :: Parser CliOptions
cliOptsParser = CliOptions
    <$> commandSubParser
    <*> argument str
        ( metavar "TARGET"
        <> help "Target directory" )
    <*> argument str
        ( metavar "FILTER"
        <> help "The filter to apply" )



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

