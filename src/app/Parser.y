{
module Parser
  (
      parseTreeSurgeon
  ) where

import Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty( NonEmpty( (:|) ), (<|))
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.Text

import qualified Lexer as X
import Lit
import qualified ParseException as PE
import RawExp
import Trace
}

%name parseTreeSurgeon exp
%tokentype { X.RangedToken }
%error { PE.parseError }
%errorhandlertype explist
%monad { X.Alex } { >>= } { pure }
%lexer { lexer } { X.RangedToken X.EOF _ }

%token
-- APP                           { X.RangedToken X.Ghost _ }
--  APP                           { _ }
-- Identifiers
  IDENT 			{ X.RangedToken (X.Identifier _) _ }
-- List
  '['        			{ X.RangedToken X.LBrack _ }
  ']'        			{ X.RangedToken X.RBrack _ }
  ','        			{ X.RangedToken X.Comma _ }
-- Literals
  FALSE     			{ X.RangedToken X.LFalse _ }
  TRUE     			{ X.RangedToken X.LTrue _ }
  STRING     			{ X.RangedToken (X.String _) _ }
  INTEGER     			{ X.RangedToken (X.Integer _) _ }
-- Syntax
  LET 				{ X.RangedToken X.Let _ }
  '=' 				{ X.RangedToken X.Eq _ }
  ';'        			{ X.RangedToken X.SemiColon _ }
  IN 				{ X.RangedToken X.In _ }
  '('        			{ X.RangedToken X.LPar _ }
  ')'        			{ X.RangedToken X.RPar _ }

-- All expressions which start an RawExp must be nonassoc
-- https://ptival.github.io/2017/05/16/parser-generators-and-function-application/

%left '!' '[' FALSE TRUE STRING LET '('
%nonassoc APP

%%

-- traceExp :: { RawExp (Trace X.Range) }
--   : exp                                 { mkTrace <$> $1 }

exp :: { RawExp X.Range }
  : single                              { $1 }
  | applyParse 		                { RawApply (info (NE.head $1) <> info (NE.last $1)) (NE.reverse $1) }

single :: { RawExp X.Range }
  -- Parentheses
  : '(' exp ')'                         { RawPar (X.rtRange $1 <> X.rtRange $3) $2 }
  -- Literals
  | lit                                 { RawLit (snd $1) (fst $1) }
  -- List
  | '[' listExpParse(exp) ']'  		{ RawList (X.rtRange $1 <> X.rtRange $3) $2 }
  -- Syntax
  | name                                { RawIdent (snd $1) (fst $1) }
  | LET decExpParse IN exp 	        { RawLet (X.rtRange $1 <> info $4) $2 $4 }
  -- Function application

applyParse :: { NE.NonEmpty (RawExp X.Range) }
  : applyParse single                   { $2 <| $1 }
  | single single                       { $2 :| [$1] }

lit :: { (Lit, X.Range) }
  -- Literals
  : bool       				{ $1 }
  | STRING        			{ unTok $1 (\rng (X.String s) -> (LString $ unQuote s, rng)) }
  | INTEGER                             { unTok $1 (\rng (X.Integer i) -> (LInteger $ fromIntegral i, rng)) }
  -- List

bool :: { (Lit, X.Range) }
  : FALSE 				{ (LBool False, X.rtRange $1) }
  | TRUE 				{ (LBool True, X.rtRange $1) }

name :: { (Text, X.Range) }
  : IDENT 				{ unTok $1 (\rng (X.Identifier n) -> (n, rng)) }

listExpParse(typ) :: { [RawExp X.Range] }
  : listExpParseRev(typ)         	{ L.reverse $1 }

listExpParseRev(typ) :: { [RawExp X.Range] }
  : listExpParseRev(typ) ',' typ 	{ $3 : $1 }
  | typ 				{ [ $1 ] }
  | 		       			{ [] }

namedExp :: { NamedExp X.Range }
  : name '=' exp  			{ (fst $1, $3) }

sepBy_rev(p, sep)
  :                                     { [] }
  | sepBy_rev(p, sep) sep p             { $3 : $1 }

sepBy(p, sep)
  : sepBy_rev(p, sep)                   { L.reverse $1 }

decExpParse :: { [NamedExp X.Range] }
  : decExpParse namedExp ';' 	        { $2 : $1 }
  | decExpParse namedExp     	        { $2 : $1 }
  | namedExp ';'	         	{ [ $1 ] }
  | namedExp                            { [ $1 ] }

{

-- | Remove quote marks from a string
unQuote :: Text -> Text
unQuote bs = pack $ unQuote' $ unpack bs

unQuote' :: String -> String
unQuote' ('\"':ss) = unQuote' $ Prelude.reverse ss
unQuote' ('\'':ss) = unQuote' $ Prelude.reverse ss
unQuote' ss = ss

-- | Build a simple node by extracting its token type and range.
unTok :: X.RangedToken -> (X.Range -> X.Token -> a) -> a
unTok (X.RangedToken tok range) ctor = ctor range tok

-- | Build a simple node by extracting its token type and range.
valOnly :: X.RangedToken -> (X.Token -> a) -> a
valOnly (X.RangedToken tok range) ctor = ctor tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

lexer :: (X.RangedToken -> X.Alex a) -> X.Alex a
lexer = (=<< X.alexMonadScan)
}
