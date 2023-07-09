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
-- Identifiers
  identifier 			{ L.RangedToken (L.Identifier _) _ }
-- Operators
  '&'				{ L.RangedToken L.And _ }
  '!'				{ L.RangedToken L.Not _ }
  '|'        			{ L.RangedToken L.Or _ }
-- List
  '['        			{ L.RangedToken L.LBrack _ }
  ']'        			{ L.RangedToken L.RBrack _ }
-- Literals
  False     			{ L.RangedToken L.LFalse _ }
  True     			{ L.RangedToken L.LTrue _ }
  string     			{ L.RangedToken (L.String _) _ }
-- Syntax
  let 				{ L.RangedToken L.Let _ }
  '=' 				{ L.RangedToken L.Eq _ }
  ';'        			{ L.RangedToken L.SemiColon _ }
  in 				{ L.RangedToken L.In _ }
  '('        			{ L.RangedToken L.LPar _ }
  ')'        			{ L.RangedToken L.RPar _ }

%right name
%left let
%right in
%left '='
%left ';'
%left '|'
%left '&'
%left '!'
%left True
%left False

%%

exp :: { Exp L.Range }
  -- Logical operators
  : exp '&' exp 			{ And (info $1 <-> info $3) $1 $3 }
  | '!' exp 				{ Not (L.rtRange $1 <-> info $2) $2 }
  | exp '|' exp 			{ Or (info $1 <-> info $3) $1 $3 }
  -- Function
  | False 				{ LBool (L.rtRange $1) False }
  | True 				{ LBool (L.rtRange $1) True }
  | name listExpParse(exp) 		{ EFunc (info $1 <-> info (last $2)) $1 $2 }
  -- Syntax
  | '(' exp ')'				{ EPar (L.rtRange $1 <-> L.rtRange $3) $2 }
  | let decExpParse(namedExp) in exp 	{ ELet (L.rtRange $1 <-> info $4) $2 $4 }
  | name 			 	{ EVar (info $1) $1 }

lit :: { Lit L.Range }
  -- Literals
  : '[' listLitParse(lit) ']'  		{ LList (L.rtRange $1 <-> L.rtRange $3) $2 }
  | string        			{ unTok $1 (\rng (L.String s) -> LString rng $ unQuote s) }
  -- Function
  | name listLitParse(lit) 		{ LFunc (info $1 <-> info (last $2)) $1 $2 }
  -- Syntax
  | '(' lit ')'				{ LPar (L.rtRange $1 <-> L.rtRange $3) $2 }
  | let decLitParse(namedLit) in lit 	{ LLet (L.rtRange $1 <-> info $4) $2 $4 }
  | name 			 	{ LVar (info $1) $1 }

name :: { VarName L.Range }
  : identifier 				{ unTok $1 (\rng (L.Identifier n) -> VarName rng n) }

listExpParse(typ) :: { [Exp L.Range] }
  : listExpParse(typ) typ 		{ $2 : $1 }
  | typ 			{ [ $1 ] }
  | 		       		{ [] }

listLitParse(typ) :: { [Lit L.Range] }
  : listLitParse(typ) typ 		{ $2 : $1 }
  | typ 			{ [ $1 ] }
  | 		       		{ [] }

namedExp :: { NamedExp L.Range }
  : name '=' exp ';'  	{ ($1, $3) }

namedLit :: { NamedLit L.Range }
  : name '=' lit ';'  	{ ($1, $3) }

decExpParse(typ) :: { [NamedExp L.Range] }
  : decExpParse(typ) typ 	{ $2 : $1 }
  | typ	 	        	{ [ $1 ] }
  | 	       			{ [] }

decLitParse(typ) :: { [NamedLit L.Range] }
  : decLitParse(typ) typ 	{ $2 : $1 }
  | typ	 	        	{ [ $1 ] }
  | 	       			{ [] }

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

-- | Build a simple node by extracting its token type and range.
valOnly :: L.RangedToken -> (L.Token -> a) -> a
valOnly (L.RangedToken tok range) ctor = ctor tok

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
