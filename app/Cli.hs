module Cli (
  CliOptions(..)
  , IncludeExclude(..)
  , Command(..)
  , cliOptsParser
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

showTreeTypeParser :: Parser Bool
showTreeTypeParser = switch (
    long "diff" <> help "Show diff between original tree and filtered tree"
    )

data IncludeExclude = Include | Exclude

inclExclParser :: Parser IncludeExclude
inclExclParser = flag Include Exclude
    ( long "exclude"
    <> help "Show what has been excluded from the original tree, rather than included" )

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

commandSubParser = subparser $ showTreeParser' <> toBashParser'
    where showTreeParser' = command "show-tree"
              $ info showTreeParser
              $ progDesc "Show the filtered tree"
          toBashParser' = command "to-bash"
              $ info toBashArrayParser
              $ progDesc "Output the files of the filtered tree as a bash array"

cliOptsParser :: Parser CliOptions
cliOptsParser = CliOptions
    <$> argument str
        ( metavar "TARGET"
        <> help "The directory on which to filter" )
    <*> argument str
        ( metavar "FILTER"
        <> help "The filter string to apply" )
    <*> commandSubParser

