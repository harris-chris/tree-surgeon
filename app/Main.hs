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
tsFilter (ShowTree target filterStr Comparative) =
    let f = \x y -> putStrLn $ showTreeComparison x y
    in applyFilterWithComparative target (BS.pack filterStr) f
tsFilter (ShowTree target filterStr FilteredOnly) =
    let f = putStrLn . showTree
    in applyFilterWith target (BS.pack filterStr) f

data ShowTreeType = Comparative | FilteredOnly

showTreeTypeParser :: Parser ShowTreeType
showTreeTypeParser = comparativeParser <|> filteredOnlyParser
     where comparativeParser = flag' Comparative (long "comparative")
           filteredOnlyParser = flag' FilteredOnly (long "filtered-only")

data CliOptions =
  ShowTree { target :: String , filterText :: String, treeType :: ShowTreeType }

cliOptsParser :: Parser CliOptions
cliOptsParser = ShowTree
      <$> argument str
          ( metavar "TARGET"
          <> help "Target directory" )
      <*> argument str
          ( metavar "FILTER"
          <> help "The filter to apply" )
      <*> showTreeTypeParser
      -- <*> option auto
      --     ( long "enthusiasm"
      --    <> help "How enthusiastically to greet"
      --    <> showDefault
      --    <> value 1
      --    <> metavar "INT" )

