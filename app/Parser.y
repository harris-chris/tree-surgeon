{
module Parser
  ( parseTreeSurgeon
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
import AST
}

%name parseTreeSurgeon exp
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%token
-- Operators
  '|'        	{ L.RangedToken L.Or _ }
  '&'		{ L.RangedToken L.And _ }
-- Matchers
  isChildOf  	{ L.RangedToken L.IsChildOf _ }
  nameEndsWith  { L.RangedToken L.NameEndsWith _ }
-- Syntax
  '('        	{ L.RangedToken L.LPar _ }
  ')'        	{ L.RangedToken L.RPar _ }
  '['        	{ L.RangedToken L.LBrack _ }
  ']'        	{ L.RangedToken L.RBrack _ }
  ','	        { L.RangedToken L.Comma _ }
-- Values
  string     	{ L.RangedToken (L.String _) _ }

%left '|'
%left '&'
%left isChildOf
%left nameEndsWith

%%

sepBy_rev(p, sep)
  :                         { [] }
  | sepBy_rev(p, sep) sep p { $3 : $1 }

sepBy(p, sep)
  : sepBy_rev(p, sep) { reverse $1 }

exp :: { Exp L.Range }
  : exp '|' exp 		{ Or (info $1 <-> info $3) $1 $3 }
  | exp '&' exp 		{ And (info $1 <-> info $3) $1 $3 }
  | isChildOf atom 		{ IsChildOf (L.rtRange $1 <-> info $2) $2 }
  | nameEndsWith atom    	{ NameEndsWith (L.rtRange $1 <-> info $2) $2 }
  | '(' exp ')'			{ EPar (L.rtRange $1 <-> L.rtRange $3) $2 }

atom :: { Exp L.Range }
  : string        		{ unTok $1 (\range (L.String str) -> EString range $ unQuote str) }
  | '(' atom ')'		{ EPar (L.rtRange $1 <-> L.rtRange $3) $2 }
  | '[' sepBy(atom, ',') ']'  	{ EList (L.rtRange $1 <-> L.rtRange $3) $2 }
{

-- | Remove quote marks from a string
unQuote :: ByteString -> ByteString
unQuote bs = BS.pack $ unQuote' $ BS.unpack bs

unQuote' :: String -> String
unQuote' ('\"':ss) = unQuote' $ reverse ss
unQuote' ss = ss

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2

parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}
