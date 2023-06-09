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
	| HappyAbsSyn7 (VarName L.Range)
	| HappyAbsSyn8 (NamedExp L.Range)
	| HappyAbsSyn9 ([NamedExp L.Range])
	| HappyAbsSyn10 ([Lit L.Range])

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
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46 :: () => Prelude.Int -> ({-HappyReduction (L.Alex) = -}
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
 happyReduce_23 :: () => ({-HappyReduction (L.Alex) = -}
	   Prelude.Int 
	-> (L.RangedToken)
	-> HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)
	-> [HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (L.Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,157) ([21504,143,15696,32770,2,0,0,0,0,0,0,0,9173,0,0,0,0,0,0,0,0,20480,573,640,0,9173,21504,143,160,4,0,0,4,0,4096,256,0,0,0,50176,143,0,16384,2292,0,0,36676,0,0,0,16,4097,0,0,0,0,0,54528,35,36692,0,0,0,0,2,0,0,32928,32768,2,0,0,36676,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTreeSurgeon","exp","lit","bool","name","namedExp","decExpParse__namedExp__","listLitParse__lit__","identifier","'&'","'!'","'|'","'['","']'","False","True","string","let","'='","';'","in","'('","')'","%eof"]
        bit_start = st Prelude.* 26
        bit_end = (st Prelude.+ 1) Prelude.* 26
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..25]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (11) = happyShift action_6
action_0 (13) = happyShift action_7
action_0 (15) = happyShift action_8
action_0 (17) = happyShift action_9
action_0 (18) = happyShift action_10
action_0 (19) = happyShift action_11
action_0 (20) = happyShift action_12
action_0 (24) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_6
action_1 (13) = happyShift action_7
action_1 (15) = happyShift action_8
action_1 (17) = happyShift action_9
action_1 (18) = happyShift action_10
action_1 (19) = happyShift action_11
action_1 (20) = happyShift action_12
action_1 (24) = happyShift action_13
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 (7) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (12) = happyShift action_15
action_2 (14) = happyShift action_16
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_4

action_4 _ = happyReduce_8

action_5 (11) = happyShift action_6
action_5 (15) = happyShift action_8
action_5 (17) = happyShift action_9
action_5 (18) = happyShift action_10
action_5 (19) = happyShift action_11
action_5 (20) = happyShift action_25
action_5 (24) = happyShift action_26
action_5 (5) = happyGoto action_22
action_5 (6) = happyGoto action_4
action_5 (7) = happyGoto action_23
action_5 (10) = happyGoto action_28
action_5 _ = happyReduce_23

action_6 _ = happyReduce_16

action_7 (11) = happyShift action_6
action_7 (13) = happyShift action_7
action_7 (15) = happyShift action_8
action_7 (17) = happyShift action_9
action_7 (18) = happyShift action_10
action_7 (19) = happyShift action_11
action_7 (20) = happyShift action_12
action_7 (24) = happyShift action_13
action_7 (4) = happyGoto action_27
action_7 (5) = happyGoto action_3
action_7 (6) = happyGoto action_4
action_7 (7) = happyGoto action_5
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (11) = happyShift action_6
action_8 (15) = happyShift action_8
action_8 (17) = happyShift action_9
action_8 (18) = happyShift action_10
action_8 (19) = happyShift action_11
action_8 (20) = happyShift action_25
action_8 (24) = happyShift action_26
action_8 (5) = happyGoto action_22
action_8 (6) = happyGoto action_4
action_8 (7) = happyGoto action_23
action_8 (10) = happyGoto action_24
action_8 _ = happyReduce_23

action_9 _ = happyReduce_14

action_10 _ = happyReduce_15

action_11 _ = happyReduce_10

action_12 (11) = happyShift action_6
action_12 (7) = happyGoto action_19
action_12 (8) = happyGoto action_20
action_12 (9) = happyGoto action_21
action_12 _ = happyReduce_20

action_13 (11) = happyShift action_6
action_13 (13) = happyShift action_7
action_13 (15) = happyShift action_8
action_13 (17) = happyShift action_9
action_13 (18) = happyShift action_10
action_13 (19) = happyShift action_11
action_13 (20) = happyShift action_12
action_13 (24) = happyShift action_13
action_13 (4) = happyGoto action_17
action_13 (5) = happyGoto action_18
action_13 (6) = happyGoto action_4
action_13 (7) = happyGoto action_5
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (12) = happyShift action_15
action_14 (14) = happyShift action_16
action_14 (26) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (11) = happyShift action_6
action_15 (13) = happyShift action_7
action_15 (15) = happyShift action_8
action_15 (17) = happyShift action_9
action_15 (18) = happyShift action_10
action_15 (19) = happyShift action_11
action_15 (20) = happyShift action_12
action_15 (24) = happyShift action_13
action_15 (4) = happyGoto action_40
action_15 (5) = happyGoto action_3
action_15 (6) = happyGoto action_4
action_15 (7) = happyGoto action_5
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (11) = happyShift action_6
action_16 (13) = happyShift action_7
action_16 (15) = happyShift action_8
action_16 (17) = happyShift action_9
action_16 (18) = happyShift action_10
action_16 (19) = happyShift action_11
action_16 (20) = happyShift action_12
action_16 (24) = happyShift action_13
action_16 (4) = happyGoto action_39
action_16 (5) = happyGoto action_3
action_16 (6) = happyGoto action_4
action_16 (7) = happyGoto action_5
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (12) = happyShift action_15
action_17 (14) = happyShift action_16
action_17 (25) = happyShift action_38
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (25) = happyShift action_37
action_18 _ = happyReduce_4

action_19 (21) = happyShift action_36
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_19

action_21 (11) = happyShift action_6
action_21 (23) = happyShift action_35
action_21 (7) = happyGoto action_19
action_21 (8) = happyGoto action_34
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_22

action_23 (11) = happyShift action_6
action_23 (15) = happyShift action_8
action_23 (17) = happyShift action_9
action_23 (18) = happyShift action_10
action_23 (19) = happyShift action_11
action_23 (20) = happyShift action_25
action_23 (24) = happyShift action_26
action_23 (5) = happyGoto action_22
action_23 (6) = happyGoto action_4
action_23 (7) = happyGoto action_23
action_23 (10) = happyGoto action_33
action_23 _ = happyReduce_23

action_24 (11) = happyShift action_6
action_24 (15) = happyShift action_8
action_24 (16) = happyShift action_32
action_24 (17) = happyShift action_9
action_24 (18) = happyShift action_10
action_24 (19) = happyShift action_11
action_24 (20) = happyShift action_25
action_24 (24) = happyShift action_26
action_24 (5) = happyGoto action_29
action_24 (6) = happyGoto action_4
action_24 (7) = happyGoto action_23
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (11) = happyShift action_6
action_25 (7) = happyGoto action_19
action_25 (8) = happyGoto action_20
action_25 (9) = happyGoto action_31
action_25 _ = happyReduce_20

action_26 (11) = happyShift action_6
action_26 (15) = happyShift action_8
action_26 (17) = happyShift action_9
action_26 (18) = happyShift action_10
action_26 (19) = happyShift action_11
action_26 (20) = happyShift action_25
action_26 (24) = happyShift action_26
action_26 (5) = happyGoto action_30
action_26 (6) = happyGoto action_4
action_26 (7) = happyGoto action_23
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_2

action_28 (11) = happyShift action_6
action_28 (12) = happyReduce_11
action_28 (14) = happyReduce_11
action_28 (15) = happyShift action_8
action_28 (17) = happyShift action_9
action_28 (18) = happyShift action_10
action_28 (19) = happyShift action_11
action_28 (20) = happyShift action_25
action_28 (22) = happyReduce_11
action_28 (24) = happyShift action_26
action_28 (25) = happyReduce_11
action_28 (26) = happyReduce_11
action_28 (5) = happyGoto action_29
action_28 (6) = happyGoto action_4
action_28 (7) = happyGoto action_23
action_28 _ = happyReduce_11

action_29 _ = happyReduce_21

action_30 (25) = happyShift action_37
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (11) = happyShift action_6
action_31 (23) = happyShift action_44
action_31 (7) = happyGoto action_19
action_31 (8) = happyGoto action_34
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_9

action_33 (11) = happyShift action_6
action_33 (15) = happyShift action_8
action_33 (17) = happyShift action_9
action_33 (18) = happyShift action_10
action_33 (19) = happyShift action_11
action_33 (20) = happyShift action_25
action_33 (24) = happyShift action_26
action_33 (5) = happyGoto action_29
action_33 (6) = happyGoto action_4
action_33 (7) = happyGoto action_23
action_33 _ = happyReduce_11

action_34 _ = happyReduce_18

action_35 (11) = happyShift action_6
action_35 (13) = happyShift action_7
action_35 (15) = happyShift action_8
action_35 (17) = happyShift action_9
action_35 (18) = happyShift action_10
action_35 (19) = happyShift action_11
action_35 (20) = happyShift action_12
action_35 (24) = happyShift action_13
action_35 (4) = happyGoto action_42
action_35 (5) = happyGoto action_43
action_35 (6) = happyGoto action_4
action_35 (7) = happyGoto action_5
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (11) = happyShift action_6
action_36 (13) = happyShift action_7
action_36 (15) = happyShift action_8
action_36 (17) = happyShift action_9
action_36 (18) = happyShift action_10
action_36 (19) = happyShift action_11
action_36 (20) = happyShift action_12
action_36 (24) = happyShift action_13
action_36 (4) = happyGoto action_41
action_36 (5) = happyGoto action_3
action_36 (6) = happyGoto action_4
action_36 (7) = happyGoto action_5
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_12

action_38 _ = happyReduce_6

action_39 (12) = happyShift action_15
action_39 _ = happyReduce_3

action_40 _ = happyReduce_1

action_41 (12) = happyShift action_15
action_41 (14) = happyShift action_16
action_41 (22) = happyShift action_46
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (12) = happyShift action_15
action_42 (14) = happyShift action_16
action_42 _ = happyReduce_7

action_43 (12) = happyReduce_13
action_43 (14) = happyReduce_13
action_43 (22) = happyReduce_13
action_43 (25) = happyReduce_13
action_43 (26) = happyReduce_13
action_43 _ = happyReduce_13

action_44 (11) = happyShift action_6
action_44 (15) = happyShift action_8
action_44 (17) = happyShift action_9
action_44 (18) = happyShift action_10
action_44 (19) = happyShift action_11
action_44 (20) = happyShift action_25
action_44 (24) = happyShift action_26
action_44 (5) = happyGoto action_45
action_44 (6) = happyGoto action_4
action_44 (7) = happyGoto action_23
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_13

action_46 _ = happyReduce_17

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
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (ELit (info happy_var_1) happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  4 happyReduction_5
happyReduction_5 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (EFunc (info happy_var_1 <-> info (last happy_var_2)) happy_var_1 happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyTerminal happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (EPar (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 4 happyReduction_7
happyReduction_7 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ELet (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyTerminal happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (LList (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (unTok happy_var_1 (\rng (L.String s) -> LString rng $ unQuote s)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  5 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (LFunc (info happy_var_1 <-> info (last happy_var_2)) happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (LPar (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 5 happyReduction_13
happyReduction_13 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LLet (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (LBool (L.rtRange happy_var_1) False
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  6 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (LBool (L.rtRange happy_var_1) True
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (unTok happy_var_1 (\rng (L.Identifier n) -> VarName rng n)
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 8 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_2  9 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([ happy_var_1 ]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  9 happyReduction_20
happyReduction_20  =  HappyAbsSyn9
		 ([]
	)

happyReduce_21 = happySpecReduce_2  10 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  10 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn10
		 ([ happy_var_1 ]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  10 happyReduction_23
happyReduction_23  =  HappyAbsSyn10
		 ([]
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken L.EOF _ -> action 26 26 tk (HappyState action) sts stk;
	L.RangedToken (L.Identifier _) _ -> cont 11;
	L.RangedToken L.And _ -> cont 12;
	L.RangedToken L.Not _ -> cont 13;
	L.RangedToken L.Or _ -> cont 14;
	L.RangedToken L.LBrack _ -> cont 15;
	L.RangedToken L.RBrack _ -> cont 16;
	L.RangedToken L.LFalse _ -> cont 17;
	L.RangedToken L.LTrue _ -> cont 18;
	L.RangedToken (L.String _) _ -> cont 19;
	L.RangedToken L.Let _ -> cont 20;
	L.RangedToken L.Eq _ -> cont 21;
	L.RangedToken L.SemiColon _ -> cont 22;
	L.RangedToken L.In _ -> cont 23;
	L.RangedToken L.LPar _ -> cont 24;
	L.RangedToken L.RPar _ -> cont 25;
	_ -> happyError' (tk, [])
	})

happyError_ explist 26 tk = happyError' (tk, explist)
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
