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

<0> $white+ 		{ skip }
-- Operators
<0> "|"			{ tok Or }
<0> "&"			{ tok And }
-- Matchers
<0> ancestorNameIs	{ tok AncestorNameIs }
<0> nameStartsWith      { tok NameStartsWith }
<0> nameEndsWith        { tok NameEndsWith }
<0> nameContains        { tok NameContains }
<0> nameIs        	{ tok NameIs }
-- Syntax
<0> "("     		{ tok LPar }
<0> ")"     		{ tok RPar }
-- List
<0> "["     		{ tok LBrack }
<0> "]"     		{ tok RBrack }
<0> ","     		{ tok Comma }
-- Values
<0> \"[^\"]*\" 		{ tokString }
-- Comments
<0> "--" .*\n 		{ skip }

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

data Token
  = Or
  | And
  | AncestorNameIs
  | String ByteString
  | LPar
  | RPar
  | EOF
  | NameEndsWith
  | NameStartsWith
  | NameContains
  | NameIs
  | LBrack
  | RBrack
  | Comma
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
