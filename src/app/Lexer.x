{
module Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , getFullRange
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  , rtRange
  , scanMany
  ) where

import Prelude hiding (length, lines)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as L
import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 (ByteString, length, lines)
import Data.Text.Encoding

import IsTrace

}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]
$strMark = [ " \' ]

@strSingle     = \' ($printable # \')* \'
@strDouble     = \" ($printable # \")* \"

@id = ($alpha | \_ | \= | \< | \> | \& | \! | \| | \+ | \-) ($alpha | $digit | \_ | \' | \? | \=)*
tokens :-

<0> $white+ 			{ skip }
-- List
<0> "["     			{ tok LBrack }
<0> "]"     			{ tok RBrack }
<0> ","     			{ tok Comma }
-- Literals
<0> "False" 			{ tok LFalse }
<0> "True" 			{ tok LTrue }
<0> @strSingle  	        { tokString }
<0> @strDouble 			{ tokString }
<0> $digit+ 			{ tokInteger }
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
<0> @id 			{ tokIdentifier }


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

-- TODO: we should not need this at all
getFullRange :: ByteString -> Range
getFullRange filterStr =
    let strLines = lines filterStr
        start' = AlexPn 0 0 0
        stop' = AlexPn 0 (L.length strLines) (fromIntegral $ length $ last strLines)
    in Range start' stop'

instance Semigroup Range where
    Range a _ <> Range _ b = Range a b

instance IsTrace Range where
    trace txt (Range (AlexPn _ lineX colX) (AlexPn _ lineY colY)) =
        reproduceFromLine txt lineX lineY

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
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

tokString :: AlexAction RangedToken
tokString inp@(_, _, str, _) len =
    pure RangedToken
        { rtToken = String $ decodeUtf8Lenient $ BS.toStrict $ BS.take len str
        , rtRange = mkRange inp len
        }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
    pure RangedToken
        { rtToken = Integer $ read $ BS.unpack $ BS.take len str
        , rtRange = mkRange inp len
        }

tokIdentifier :: AlexAction RangedToken
tokIdentifier inp@(_, _, str, _) len =
    pure RangedToken
        { rtToken = Identifier $ decodeUtf8Lenient $ BS.toStrict $ BS.take len str
        , rtRange = mkRange inp len
        }

data Token
  -- Identiifers
  = Identifier T.Text
  -- Equals
  | Eqs
  -- Literals
  | String T.Text
  | Integer Integer
  | LFalse
  | LTrue
  -- Parentheses
  | LPar
  | RPar
  -- Lists
  | LBrack
  | RBrack
  | Comma
  -- Syntax
  | Let
  | Eq
  | In
  | SemiColon
  -- Ghost
  | Ghost
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
