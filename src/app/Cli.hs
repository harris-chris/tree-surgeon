module Cli (
  CliOptions(..)
  , PathsType(..)
  , TypeConstraint(..)
  , cliOptsParser
  ) where

import Options.Applicative

data CliOptions
    = ShowFilteredTree
        { sftFilter :: Either String String
        , sftSource :: String }
    | ShowDiffTree
        { sdtFilter :: Either String String
        , sdtSource :: String }
    | ToBashArray
        { tbaFilter :: Either String String
        , tbaSource :: String
        , tbaExclude :: Bool
        , tbaOptTypeConstraint :: Maybe TypeConstraint
        , tbaPathsType :: PathsType }
    | ToGitExclude
        { tgeFilter :: Either String String }

data PathsType = RelToSource | Absolute

data TypeConstraint = FilesOnly | DirsOnly | AlwaysDirs

cliOptsParser :: Parser CliOptions
cliOptsParser =
    hsubparser
        (command
             "tree"
             (info showFilteredTree (progDesc showDesc))
        <> command
             "tree-diff"
             (info showDiffTree (progDesc showDiffDesc))
        <> command
             "to-bash"
             (info toBash (progDesc toBashDesc))
        <> command
             "to-gitignore"
             (info toGitExclude (progDesc toGitExcludeDesc))
        )
    where
        showDesc = "Show standard tree output for a directory tree after filtering it "
            <> "with a filter expression"
        showDiffDesc = "Show the before and after diff of a directory tree that has been "
            <> "filtered with a filter expression"
        toBashDesc = "Filter a tree and print the resulting output as a bash array"
        toGitExcludeDesc = "Convert a tree-surgeon expression to a git exlusion pattern, for "
            <> "use in, eg, a .gitignore file"

showFilteredTree :: Parser CliOptions
showFilteredTree = ShowFilteredTree <$> filterStrArg <*> sourceDirArg

showDiffTree :: Parser CliOptions
showDiffTree = ShowDiffTree <$> filterStrArg <*> sourceDirArg

toBash :: Parser CliOptions
toBash = ToBashArray
            <$> filterStrArg
            <*> sourceDirArg
            <*> excludedArg
            <*> optTypeConstraintArg
            <*> pathsArg

toGitExclude :: Parser CliOptions
toGitExclude = ToGitExclude
            <$> filterStrArg

excludedArg :: Parser Bool
excludedArg = switch
    $ long "excluded"
    <> short 'e'
    <> help "Output excluded, rather than included, files and directories"

pathsArg :: Parser PathsType
pathsArg =
    option pathsParse (
        ( long "paths" )
        <> ( help $ "Control how paths of included files are shown; "
                 <> "'s' or 'source' to show paths relative to the source directory "
                 <> "'a' or 'absolute' to show absolute paths. "
                 <> "Default value is 'source'"
                 )
        ) <|> pure RelToSource -- the default option

pathsParse :: ReadM PathsType
pathsParse = str >>= \s -> case s of
    "s"             -> return RelToSource
    "source"        -> return RelToSource
    "a"             -> return Absolute
    "absolute"      -> return Absolute
    _ -> readerError
            "Accepted --paths values are 's'/'source' or 'a'/'absolute'"

optTypeConstraintArg :: Parser (Maybe TypeConstraint)
optTypeConstraintArg =
    optional $ option typeConstraintParse (
        long "type"
        <> ( help $ "Constrain the type of object shown in the output; "
                 <> "'f' or 'files' for only regular files, "
                 <> "'d' or 'directories' for only directories "
                 <> "'a' or 'alwaysdirs' to show both included directories, and, as individual "
                 <> "array elements, all parent directories of all included files"
                 )
        )

typeConstraintParse :: ReadM TypeConstraint
typeConstraintParse = str >>= \s -> case s of
    "f"             -> return FilesOnly
    "files"         -> return FilesOnly
    "d"             -> return DirsOnly
    "directories"   -> return DirsOnly
    "a"             -> return AlwaysDirs
    "alwaysdirs"    -> return AlwaysDirs
    _ -> readerError
            "Accepted --type values are 'd'/'directories', 'f'/'files' or 'a'/'alwaysdirs'"

filterStrArg :: Parser (Either String String)
filterStrArg =
    (Left <$> strOption
        (long "string"
        <> short 'f'
        <> metavar "FILTER_STRING"
        <> help "Filter to apply to folder"))
    <|> (Right <$> strOption
        (long "file"
        <> metavar "FILTER_FILE"
        <> help "File that contains filter to apply to folder"))

sourceDirArg :: Parser String
sourceDirArg =
  strOption
    (long "source" <> short 's' <> metavar "SOURCE" <> help "Source directory to apply filter to")

