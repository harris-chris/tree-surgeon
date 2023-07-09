{-# OPTIONS_GHC -w #-}
module Parser
  ( parseTreeSurgeon
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
import AST
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (L.RangedToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Exp L.Range)
	| HappyAbsSyn5 (Lit L.Range)
	| HappyAbsSyn6 (VarName L.Range)
	| HappyAbsSyn7 (NamedExp L.Range)
	| HappyAbsSyn8 (NamedLit L.Range)
	| HappyAbsSyn9 ([NamedExp L.Range])
	| HappyAbsSyn10 ([NamedLit L.Range])
	| HappyAbsSyn11 ([Exp L.Range])
	| HappyAbsSyn12 ([Lit L.Range])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (L.RangedToken)
	-> HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
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
 action_29 :: () => Prelude.Int -> ({-HappyReduction (L.Alex) = -}
	   Prelude.Int 
	-> (L.RangedToken)
	-> HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)
	-> [HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (L.Alex) HappyAbsSyn)

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
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30 :: () => ({-HappyReduction (L.Alex) = -}
	   Prelude.Int 
	-> (L.RangedToken)
	-> HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)
	-> [HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (L.Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,75) ([20480,556,50432,34,160,0,0,0,0,50432,34,0,0,0,0,0,50432,34,160,0,8901,20480,556,2560,64,16384,0,0,4096,256,0,0,0,0,0,0,0,0,0,11344,2,8901,0,0,512,0,0,0,2058,40960,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTreeSurgeon","exp","lit","name","namedExp","namedLit","decExpParse__namedExp__","decLitParse__namedLit__","listExpParse__exp__","listLitParse__lit__","identifier","'&'","'!'","'|'","'['","']'","False","True","string","let","'='","';'","in","'('","')'","%eof"]
        bit_start = st Prelude.* 28
        bit_end = (st Prelude.+ 1) Prelude.* 28
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..27]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (13) = happyShift action_4
action_0 (15) = happyShift action_5
action_0 (19) = happyShift action_6
action_0 (20) = happyShift action_7
action_0 (22) = happyShift action_8
action_0 (26) = happyShift action_9
action_0 (4) = happyGoto action_10
action_0 (6) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (13) = happyShift action_4
action_1 (15) = happyShift action_5
action_1 (19) = happyShift action_6
action_1 (20) = happyShift action_7
action_1 (22) = happyShift action_8
action_1 (26) = happyShift action_9
action_1 (4) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (14) = happyShift action_11
action_2 (16) = happyShift action_12
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (13) = happyShift action_4
action_3 (14) = happyReduce_27
action_3 (15) = happyShift action_5
action_3 (16) = happyReduce_27
action_3 (19) = happyShift action_6
action_3 (20) = happyShift action_7
action_3 (22) = happyShift action_8
action_3 (24) = happyReduce_27
action_3 (26) = happyShift action_9
action_3 (27) = happyReduce_27
action_3 (28) = happyReduce_27
action_3 (4) = happyGoto action_18
action_3 (6) = happyGoto action_3
action_3 (11) = happyGoto action_19
action_3 _ = happyReduce_27

action_4 _ = happyReduce_16

action_5 (13) = happyShift action_4
action_5 (15) = happyShift action_5
action_5 (19) = happyShift action_6
action_5 (20) = happyShift action_7
action_5 (22) = happyShift action_8
action_5 (26) = happyShift action_9
action_5 (4) = happyGoto action_17
action_5 (6) = happyGoto action_3
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_4

action_7 _ = happyReduce_5

action_8 (13) = happyShift action_4
action_8 (6) = happyGoto action_14
action_8 (7) = happyGoto action_15
action_8 (9) = happyGoto action_16
action_8 _ = happyReduce_21

action_9 (13) = happyShift action_4
action_9 (15) = happyShift action_5
action_9 (19) = happyShift action_6
action_9 (20) = happyShift action_7
action_9 (22) = happyShift action_8
action_9 (26) = happyShift action_9
action_9 (4) = happyGoto action_13
action_9 (6) = happyGoto action_3
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (14) = happyShift action_11
action_10 (16) = happyShift action_12
action_10 (28) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (13) = happyShift action_4
action_11 (15) = happyShift action_5
action_11 (19) = happyShift action_6
action_11 (20) = happyShift action_7
action_11 (22) = happyShift action_8
action_11 (26) = happyShift action_9
action_11 (4) = happyGoto action_26
action_11 (6) = happyGoto action_3
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (13) = happyShift action_4
action_12 (15) = happyShift action_5
action_12 (19) = happyShift action_6
action_12 (20) = happyShift action_7
action_12 (22) = happyShift action_8
action_12 (26) = happyShift action_9
action_12 (4) = happyGoto action_25
action_12 (6) = happyGoto action_3
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (14) = happyShift action_11
action_13 (16) = happyShift action_12
action_13 (27) = happyShift action_24
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (23) = happyShift action_23
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_20

action_16 (13) = happyShift action_4
action_16 (25) = happyShift action_22
action_16 (6) = happyGoto action_14
action_16 (7) = happyGoto action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_2

action_18 (14) = happyShift action_11
action_18 (16) = happyShift action_12
action_18 _ = happyReduce_26

action_19 (13) = happyShift action_4
action_19 (15) = happyShift action_5
action_19 (19) = happyShift action_6
action_19 (20) = happyShift action_7
action_19 (22) = happyShift action_8
action_19 (26) = happyShift action_9
action_19 (4) = happyGoto action_20
action_19 (6) = happyGoto action_3
action_19 _ = happyReduce_6

action_20 (14) = happyShift action_11
action_20 (16) = happyShift action_12
action_20 _ = happyReduce_25

action_21 _ = happyReduce_19

action_22 (13) = happyShift action_4
action_22 (15) = happyShift action_5
action_22 (19) = happyShift action_6
action_22 (20) = happyShift action_7
action_22 (22) = happyShift action_8
action_22 (26) = happyShift action_9
action_22 (4) = happyGoto action_28
action_22 (6) = happyGoto action_3
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (13) = happyShift action_4
action_23 (15) = happyShift action_5
action_23 (19) = happyShift action_6
action_23 (20) = happyShift action_7
action_23 (22) = happyShift action_8
action_23 (26) = happyShift action_9
action_23 (4) = happyGoto action_27
action_23 (6) = happyGoto action_3
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_7

action_25 (14) = happyShift action_11
action_25 _ = happyReduce_3

action_26 _ = happyReduce_1

action_27 (14) = happyShift action_11
action_27 (16) = happyShift action_12
action_27 (24) = happyShift action_29
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (14) = happyShift action_11
action_28 (16) = happyShift action_12
action_28 _ = happyReduce_8

action_29 _ = happyReduce_17

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And (info happy_var_1 <-> info happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (Not (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or (info happy_var_1 <-> info happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (LBool (L.rtRange happy_var_1) False
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (LBool (L.rtRange happy_var_1) True
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  4 happyReduction_6
happyReduction_6 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (EFunc (info happy_var_1 <-> info (last happy_var_2)) happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyTerminal happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (EPar (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ELet (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  4 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (EVar (info happy_var_1) happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_3)
	(HappyAbsSyn12  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (LList (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (unTok happy_var_1 (\rng (L.String s) -> LString rng $ unQuote s)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (LFunc (info happy_var_1 <-> info (last happy_var_2)) happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (LPar (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 5 happyReduction_14
happyReduction_14 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LLet (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  5 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (LVar (info happy_var_1) happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (unTok happy_var_1 (\rng (L.Identifier n) -> VarName rng n)
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 7 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 8 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  9 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  9 happyReduction_20
happyReduction_20 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn9
		 ([ happy_var_1 ]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  9 happyReduction_21
happyReduction_21  =  HappyAbsSyn9
		 ([]
	)

happyReduce_22 = happySpecReduce_2  10 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  10 happyReduction_23
happyReduction_23 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 ([ happy_var_1 ]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  10 happyReduction_24
happyReduction_24  =  HappyAbsSyn10
		 ([]
	)

happyReduce_25 = happySpecReduce_2  11 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  11 happyReduction_26
happyReduction_26 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn11
		 ([ happy_var_1 ]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  11 happyReduction_27
happyReduction_27  =  HappyAbsSyn11
		 ([]
	)

happyReduce_28 = happySpecReduce_2  12 happyReduction_28
happyReduction_28 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_2 : happy_var_1
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn12
		 ([ happy_var_1 ]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  12 happyReduction_30
happyReduction_30  =  HappyAbsSyn12
		 ([]
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken L.EOF _ -> action 28 28 tk (HappyState action) sts stk;
	L.RangedToken (L.Identifier _) _ -> cont 13;
	L.RangedToken L.And _ -> cont 14;
	L.RangedToken L.Not _ -> cont 15;
	L.RangedToken L.Or _ -> cont 16;
	L.RangedToken L.LBrack _ -> cont 17;
	L.RangedToken L.RBrack _ -> cont 18;
	L.RangedToken L.LFalse _ -> cont 19;
	L.RangedToken L.LTrue _ -> cont 20;
	L.RangedToken (L.String _) _ -> cont 21;
	L.RangedToken L.Let _ -> cont 22;
	L.RangedToken L.Eq _ -> cont 23;
	L.RangedToken L.SemiColon _ -> cont 24;
	L.RangedToken L.In _ -> cont 25;
	L.RangedToken L.LPar _ -> cont 26;
	L.RangedToken L.RPar _ -> cont 27;
	_ -> happyError' (tk, [])
	})

happyError_ explist 28 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => L.Alex a -> (a -> L.Alex b) -> L.Alex b
happyThen = (>>=)
happyReturn :: () => a -> L.Alex a
happyReturn = (pure)
happyThen1 :: () => L.Alex a -> (a -> L.Alex b) -> L.Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> L.Alex a
happyReturn1 = happyReturn
happyError' :: () => ((L.RangedToken), [Prelude.String]) -> L.Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parseTreeSurgeon = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
