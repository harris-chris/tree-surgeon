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
    opts = info (optParser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

tsFilter :: CliOptions -> IO ()
tsFilter (CliOptions target filterStr) =
    let f = putStrLn . (showTree "" True)
    in applyFilterWith target (BS.pack filterStr) f

data CliOptions = CliOptions
  { target     :: String
  , filterText :: String }

optParser :: Parser CliOptions
optParser = CliOptions
      <$> argument str
          ( metavar "TARGET"
          <> help "Target directory" )
      <*> argument str
          ( metavar "FILTER"
          <> help "The filter to apply" )
      -- <*> option auto
      --     ( long "enthusiasm"
      --    <> help "How enthusiastically to greet"
      --    <> showDefault
      --    <> value 1
      --    <> metavar "INT" )

