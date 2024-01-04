{-# OPTIONS_GHC -w #-}
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
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (X.RangedToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (RawExp X.Range)
	| HappyAbsSyn6 (NE.NonEmpty (RawExp X.Range))
	| HappyAbsSyn7 ((Lit, X.Range))
	| HappyAbsSyn9 ((Text, X.Range))
	| HappyAbsSyn10 (NamedExp X.Range)
	| HappyAbsSyn11 ([NamedExp X.Range])
	| HappyAbsSyn12 ([RawExp X.Range])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (X.RangedToken)
	-> HappyState (X.RangedToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (X.RangedToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36 :: () => Prelude.Int -> ({-HappyReduction (X.Alex) = -}
	   Prelude.Int 
	-> (X.RangedToken)
	-> HappyState (X.RangedToken) (HappyStk HappyAbsSyn -> (X.Alex) HappyAbsSyn)
	-> [HappyState (X.RangedToken) (HappyStk HappyAbsSyn -> (X.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (X.Alex) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24 :: () => ({-HappyReduction (X.Alex) = -}
	   Prelude.Int 
	-> (X.RangedToken)
	-> HappyState (X.RangedToken) (HappyStk HappyAbsSyn -> (X.Alex) HappyAbsSyn)
	-> [HappyState (X.RangedToken) (HappyStk HappyAbsSyn -> (X.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (X.Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,84) ([24576,574,58880,35,0,0,0,0,0,0,0,0,0,9190,0,0,0,0,0,0,0,8192,0,58880,35,0,0,0,0,0,0,0,0,0,16384,0,64,0,8,32,1,0,32768,0,4096,0,15968,2,0,0,128,58880,35,0,0,9190,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTreeSurgeon","exp","single","applyParse","lit","bool","name","namedExp","decExpParse","listExpParse__exp__","listExpParseRev__exp__","IDENT","'['","']'","','","FALSE","TRUE","STRING","INTEGER","LET","'='","';'","IN","'('","')'","%eof"]
        bit_start = st Prelude.* 28
        bit_end = (st Prelude.+ 1) Prelude.* 28
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..27]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (14) = happyShift action_6
action_0 (15) = happyShift action_7
action_0 (18) = happyShift action_8
action_0 (19) = happyShift action_9
action_0 (20) = happyShift action_10
action_0 (21) = happyShift action_11
action_0 (22) = happyShift action_12
action_0 (26) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_15
action_0 (6) = happyGoto action_16
action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (14) = happyShift action_6
action_1 (15) = happyShift action_7
action_1 (18) = happyShift action_8
action_1 (19) = happyShift action_9
action_1 (20) = happyShift action_10
action_1 (21) = happyShift action_11
action_1 (22) = happyShift action_12
action_1 (26) = happyShift action_13
action_1 (5) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 (8) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_4

action_4 _ = happyReduce_10

action_5 _ = happyReduce_6

action_6 _ = happyReduce_15

action_7 (14) = happyShift action_6
action_7 (15) = happyShift action_7
action_7 (18) = happyShift action_8
action_7 (19) = happyShift action_9
action_7 (20) = happyShift action_10
action_7 (21) = happyShift action_11
action_7 (22) = happyShift action_12
action_7 (26) = happyShift action_13
action_7 (4) = happyGoto action_23
action_7 (5) = happyGoto action_15
action_7 (6) = happyGoto action_16
action_7 (7) = happyGoto action_3
action_7 (8) = happyGoto action_4
action_7 (9) = happyGoto action_5
action_7 (12) = happyGoto action_24
action_7 (13) = happyGoto action_25
action_7 _ = happyReduce_24

action_8 _ = happyReduce_13

action_9 _ = happyReduce_14

action_10 _ = happyReduce_11

action_11 _ = happyReduce_12

action_12 (14) = happyShift action_6
action_12 (9) = happyGoto action_20
action_12 (10) = happyGoto action_21
action_12 (11) = happyGoto action_22
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (14) = happyShift action_6
action_13 (15) = happyShift action_7
action_13 (18) = happyShift action_8
action_13 (19) = happyShift action_9
action_13 (20) = happyShift action_10
action_13 (21) = happyShift action_11
action_13 (22) = happyShift action_12
action_13 (26) = happyShift action_13
action_13 (4) = happyGoto action_19
action_13 (5) = happyGoto action_15
action_13 (6) = happyGoto action_16
action_13 (7) = happyGoto action_3
action_13 (8) = happyGoto action_4
action_13 (9) = happyGoto action_5
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (28) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (14) = happyShift action_6
action_15 (15) = happyShift action_7
action_15 (18) = happyShift action_8
action_15 (19) = happyShift action_9
action_15 (20) = happyShift action_10
action_15 (21) = happyShift action_11
action_15 (22) = happyShift action_12
action_15 (26) = happyShift action_13
action_15 (5) = happyGoto action_18
action_15 (7) = happyGoto action_3
action_15 (8) = happyGoto action_4
action_15 (9) = happyGoto action_5
action_15 _ = happyReduce_1

action_16 (14) = happyShift action_6
action_16 (15) = happyShift action_7
action_16 (18) = happyShift action_8
action_16 (19) = happyShift action_9
action_16 (20) = happyShift action_10
action_16 (21) = happyShift action_11
action_16 (22) = happyShift action_12
action_16 (26) = happyShift action_13
action_16 (5) = happyGoto action_17
action_16 (7) = happyGoto action_3
action_16 (8) = happyGoto action_4
action_16 (9) = happyGoto action_5
action_16 _ = happyReduce_2

action_17 _ = happyReduce_8

action_18 _ = happyReduce_9

action_19 (27) = happyShift action_32
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (23) = happyShift action_31
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (24) = happyShift action_30
action_21 _ = happyReduce_20

action_22 (14) = happyShift action_6
action_22 (25) = happyShift action_29
action_22 (9) = happyGoto action_20
action_22 (10) = happyGoto action_28
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_23

action_24 (16) = happyShift action_27
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (17) = happyShift action_26
action_25 _ = happyReduce_21

action_26 (14) = happyShift action_6
action_26 (15) = happyShift action_7
action_26 (18) = happyShift action_8
action_26 (19) = happyShift action_9
action_26 (20) = happyShift action_10
action_26 (21) = happyShift action_11
action_26 (22) = happyShift action_12
action_26 (26) = happyShift action_13
action_26 (4) = happyGoto action_36
action_26 (5) = happyGoto action_15
action_26 (6) = happyGoto action_16
action_26 (7) = happyGoto action_3
action_26 (8) = happyGoto action_4
action_26 (9) = happyGoto action_5
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_5

action_28 (24) = happyShift action_35
action_28 _ = happyReduce_18

action_29 (14) = happyShift action_6
action_29 (15) = happyShift action_7
action_29 (18) = happyShift action_8
action_29 (19) = happyShift action_9
action_29 (20) = happyShift action_10
action_29 (21) = happyShift action_11
action_29 (22) = happyShift action_12
action_29 (26) = happyShift action_13
action_29 (4) = happyGoto action_34
action_29 (5) = happyGoto action_15
action_29 (6) = happyGoto action_16
action_29 (7) = happyGoto action_3
action_29 (8) = happyGoto action_4
action_29 (9) = happyGoto action_5
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_19

action_31 (14) = happyShift action_6
action_31 (15) = happyShift action_7
action_31 (18) = happyShift action_8
action_31 (19) = happyShift action_9
action_31 (20) = happyShift action_10
action_31 (21) = happyShift action_11
action_31 (22) = happyShift action_12
action_31 (26) = happyShift action_13
action_31 (4) = happyGoto action_33
action_31 (5) = happyGoto action_15
action_31 (6) = happyGoto action_16
action_31 (7) = happyGoto action_3
action_31 (8) = happyGoto action_4
action_31 (9) = happyGoto action_5
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_3

action_33 _ = happyReduce_16

action_34 _ = happyReduce_7

action_35 _ = happyReduce_17

action_36 _ = happyReduce_22

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (RawApply (info (NE.head happy_var_1) <> info (NE.last happy_var_1)) (NE.reverse happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (RawPar (X.rtRange happy_var_1 <> X.rtRange happy_var_3) happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (RawLit (snd happy_var_1) (fst happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_3)
	(HappyAbsSyn12  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (RawList (X.rtRange happy_var_1 <> X.rtRange happy_var_3) happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (RawIdent (snd happy_var_1) (fst happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 5 happyReduction_7
happyReduction_7 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (RawLet (X.rtRange happy_var_1 <> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 <| happy_var_1
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  6 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 :| [happy_var_1]
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (unTok happy_var_1 (\rng (X.String s) -> (LString $ unQuote s, rng))
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (unTok happy_var_1 (\rng (X.Integer i) -> (LInteger $ fromIntegral i, rng))
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 ((LBool False, X.rtRange happy_var_1)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 ((LBool True, X.rtRange happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (unTok happy_var_1 (\rng (X.Identifier n) -> (n, rng))
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((fst happy_var_1, happy_var_3)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  11 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  11 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([ happy_var_1 ]
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([ happy_var_1 ]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (L.reverse happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  13 happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  13 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn12
		 ([ happy_var_1 ]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  13 happyReduction_24
happyReduction_24  =  HappyAbsSyn12
		 ([]
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	X.RangedToken X.EOF _ -> action 28 28 tk (HappyState action) sts stk;
	X.RangedToken (X.Identifier _) _ -> cont 14;
	X.RangedToken X.LBrack _ -> cont 15;
	X.RangedToken X.RBrack _ -> cont 16;
	X.RangedToken X.Comma _ -> cont 17;
	X.RangedToken X.LFalse _ -> cont 18;
	X.RangedToken X.LTrue _ -> cont 19;
	X.RangedToken (X.String _) _ -> cont 20;
	X.RangedToken (X.Integer _) _ -> cont 21;
	X.RangedToken X.Let _ -> cont 22;
	X.RangedToken X.Eq _ -> cont 23;
	X.RangedToken X.SemiColon _ -> cont 24;
	X.RangedToken X.In _ -> cont 25;
	X.RangedToken X.LPar _ -> cont 26;
	X.RangedToken X.RPar _ -> cont 27;
	_ -> happyError' (tk, [])
	})

happyError_ explist 28 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => X.Alex a -> (a -> X.Alex b) -> X.Alex b
happyThen = (>>=)
happyReturn :: () => a -> X.Alex a
happyReturn = (pure)
happyThen1 :: () => X.Alex a -> (a -> X.Alex b) -> X.Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> X.Alex a
happyReturn1 = happyReturn
happyError' :: () => ((X.RangedToken), [Prelude.String]) -> X.Alex a
happyError' tk = PE.parseError tk
parseTreeSurgeon = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
