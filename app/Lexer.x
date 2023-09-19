{
module Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  , scanMany
  ) where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

tokens :-

<0> $white+ 			{ skip }
-- Logical operators
<0> "&"				{ tok And }
<0> "!"				{ tok Not }
<0> "|"				{ tok Or }
-- List
<0> "["     			{ tok LBrack }
<0> "]"     			{ tok RBrack }
-- Literals
<0> "file" 			{ tok LFile }
<0> "False" 			{ tok LFalse }
<0> "True" 			{ tok LTrue }
<0> \"[^\"]*\" 			{ tokString }
-- Syntax
<0> "let"			{ tok Let }
<0> "="				{ tok Eq }
<0> ";"     			{ tok SemiColon }
<0> "in"			{ tok In }
<0> "("     			{ tok LPar }
<0> ")"     			{ tok RPar }
-- Comments
<0> "--" .*\n 			{ skip }
-- Identifiers
<0> ([a-zA-Z] | \_) ([a-zA-Z] | [0-9] | \_ | \' | \?)* 		{ tokIdentifier }

{
-- At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.
data AlexUserState = AlexUserState
  {
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokString inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = String $ BS.take len str
    , rtRange = mkRange inp len
    }

tokIdentifier inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier $ BS.take len str
    , rtRange = mkRange inp len
    }

data Token
  -- Identiifers
  = Identifier ByteString
  -- Logical Operators
  | And
  | Not
  | Or
  -- Literals
  | String ByteString
  | LFile
  | LFalse
  | LTrue
  -- Parentheses
  | LPar
  | RPar
  -- Lists
  | LBrack
  | RBrack
  -- Syntax
  | Let
  | Eq
  | In
  | SemiColon
  -- EOF
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go

}
