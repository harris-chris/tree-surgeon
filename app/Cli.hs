module Cli (
  CliOptions(..)
  , cliOptsParser
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

import TreeFilter

data CliOptions
    = ShowFilteredTree
        { filter :: Either String String
        , source :: String }
    | ShowDiffTree
        { filter :: Either String String
        , source :: String }
    | ToBashArray
        { filter :: Either String String
        , source :: String
        , exclude :: Bool }
    | WriteFilteredTree
        { filter :: Either String String
        , source :: String
        , destination :: String }
    deriving (Show)

cliOptsParser :: Parser CliOptions
cliOptsParser =
  hsubparser
    (command
       "show"
       (info showFilteredTree (progDesc "Show Tree")) <>
    command
       "show-diff"
       (info showDiffTree (progDesc "Show Tree Diff")) <>
    commandGroup "Tree comands:"  <> (metavar "Tree COMMAND")) <|>
  hsubparser
    (command
       "to-bash"
       (info toBash (progDesc "Filter a tree and convert the output to a bash array")) <>
    command
       "write"
       (info writeFilteredTree (progDesc "Write Tree")) <>
    commandGroup "Bash commands:"  <> (metavar "Bash COMMAND"))

showFilteredTree :: Parser CliOptions
showFilteredTree = ShowFilteredTree <$> filterStrArg <*> sourceDirArg

writeFilteredTree :: Parser CliOptions
writeFilteredTree = WriteFilteredTree <$> filterStrArg <*> sourceDirArg <*> destinationDirArg

showDiffTree :: Parser CliOptions
showDiffTree = ShowDiffTree <$> filterStrArg <*> sourceDirArg

toBash :: Parser CliOptions
toBash = ToBashArray <$> filterStrArg <*> sourceDirArg <*> excludedArg

excludedArg :: Parser Bool
excludedArg = switch
    $ long "excluded"
    <> short 'e'
    <> help "Output excluded, rather than included, files"

filterStrArg :: Parser (Either String String)
filterStrArg =
    (Left <$> strOption
        (long "filter"
        <> short 'f'
        <> metavar "FILTER STRING"
        <> help "Filter to apply to folder")
    <|>) (Right <$> strOption
        (long "file"
        <> metavar "FILTER FILE"
        <> help "File that contains filter to apply to folder"))

sourceDirArg :: Parser String
sourceDirArg =
  strOption
    (long "source" <> short 's' <> metavar "SOURCE" <> help "Source directory to apply filter to")

destinationDirArg :: Parser String
destinationDirArg =
  strOption
    (long "destination" <> short 'd' <> metavar "DESTINATION" <> help "Destination directory to write to")

