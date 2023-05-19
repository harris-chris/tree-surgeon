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
	| HappyAbsSyn5 ([Exp L.Range])

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
 action_35 :: () => Prelude.Int -> ({-HappyReduction (L.Alex) = -}
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
 happyReduce_18 :: () => ({-HappyReduction (L.Alex) = -}
	   Prelude.Int 
	-> (L.RangedToken)
	-> HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)
	-> [HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (L.Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,60) ([65408,49189,4863,24,61440,1215,24568,64514,303,38910,65280,32843,9727,65472,57362,2431,49136,63492,607,12284,1,49152,0,65408,49189,4863,24,0,768,8198,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65408,37,0,16,3072,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTreeSurgeon","exp","listParse__exp__","'|'","'&'","'!'","ancestorNameIs","ancestorNameStartsWith","ancestorNameEndsWith","ancestorNameContains","nameIs","nameStartsWith","nameEndsWith","nameContains","'('","')'","'['","']'","','","string","%eof"]
        bit_start = st Prelude.* 23
        bit_end = (st Prelude.+ 1) Prelude.* 23
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..22]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (8) = happyShift action_3
action_0 (9) = happyShift action_4
action_0 (10) = happyShift action_5
action_0 (11) = happyShift action_6
action_0 (12) = happyShift action_7
action_0 (13) = happyShift action_8
action_0 (14) = happyShift action_9
action_0 (15) = happyShift action_10
action_0 (16) = happyShift action_11
action_0 (17) = happyShift action_12
action_0 (19) = happyShift action_13
action_0 (22) = happyShift action_14
action_0 (4) = happyGoto action_15
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_3
action_1 (9) = happyShift action_4
action_1 (10) = happyShift action_5
action_1 (11) = happyShift action_6
action_1 (12) = happyShift action_7
action_1 (13) = happyShift action_8
action_1 (14) = happyShift action_9
action_1 (15) = happyShift action_10
action_1 (16) = happyShift action_11
action_1 (17) = happyShift action_12
action_1 (19) = happyShift action_13
action_1 (22) = happyShift action_14
action_1 (4) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (6) = happyShift action_16
action_2 (7) = happyShift action_17
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (8) = happyShift action_3
action_3 (9) = happyShift action_4
action_3 (10) = happyShift action_5
action_3 (11) = happyShift action_6
action_3 (12) = happyShift action_7
action_3 (13) = happyShift action_8
action_3 (14) = happyShift action_9
action_3 (15) = happyShift action_10
action_3 (16) = happyShift action_11
action_3 (17) = happyShift action_12
action_3 (19) = happyShift action_13
action_3 (22) = happyShift action_14
action_3 (4) = happyGoto action_29
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (8) = happyShift action_3
action_4 (9) = happyShift action_4
action_4 (10) = happyShift action_5
action_4 (11) = happyShift action_6
action_4 (12) = happyShift action_7
action_4 (13) = happyShift action_8
action_4 (14) = happyShift action_9
action_4 (15) = happyShift action_10
action_4 (16) = happyShift action_11
action_4 (17) = happyShift action_12
action_4 (19) = happyShift action_13
action_4 (22) = happyShift action_14
action_4 (4) = happyGoto action_28
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (8) = happyShift action_3
action_5 (9) = happyShift action_4
action_5 (10) = happyShift action_5
action_5 (11) = happyShift action_6
action_5 (12) = happyShift action_7
action_5 (13) = happyShift action_8
action_5 (14) = happyShift action_9
action_5 (15) = happyShift action_10
action_5 (16) = happyShift action_11
action_5 (17) = happyShift action_12
action_5 (19) = happyShift action_13
action_5 (22) = happyShift action_14
action_5 (4) = happyGoto action_27
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (8) = happyShift action_3
action_6 (9) = happyShift action_4
action_6 (10) = happyShift action_5
action_6 (11) = happyShift action_6
action_6 (12) = happyShift action_7
action_6 (13) = happyShift action_8
action_6 (14) = happyShift action_9
action_6 (15) = happyShift action_10
action_6 (16) = happyShift action_11
action_6 (17) = happyShift action_12
action_6 (19) = happyShift action_13
action_6 (22) = happyShift action_14
action_6 (4) = happyGoto action_26
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (8) = happyShift action_3
action_7 (9) = happyShift action_4
action_7 (10) = happyShift action_5
action_7 (11) = happyShift action_6
action_7 (12) = happyShift action_7
action_7 (13) = happyShift action_8
action_7 (14) = happyShift action_9
action_7 (15) = happyShift action_10
action_7 (16) = happyShift action_11
action_7 (17) = happyShift action_12
action_7 (19) = happyShift action_13
action_7 (22) = happyShift action_14
action_7 (4) = happyGoto action_25
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (8) = happyShift action_3
action_8 (9) = happyShift action_4
action_8 (10) = happyShift action_5
action_8 (11) = happyShift action_6
action_8 (12) = happyShift action_7
action_8 (13) = happyShift action_8
action_8 (14) = happyShift action_9
action_8 (15) = happyShift action_10
action_8 (16) = happyShift action_11
action_8 (17) = happyShift action_12
action_8 (19) = happyShift action_13
action_8 (22) = happyShift action_14
action_8 (4) = happyGoto action_24
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (8) = happyShift action_3
action_9 (9) = happyShift action_4
action_9 (10) = happyShift action_5
action_9 (11) = happyShift action_6
action_9 (12) = happyShift action_7
action_9 (13) = happyShift action_8
action_9 (14) = happyShift action_9
action_9 (15) = happyShift action_10
action_9 (16) = happyShift action_11
action_9 (17) = happyShift action_12
action_9 (19) = happyShift action_13
action_9 (22) = happyShift action_14
action_9 (4) = happyGoto action_23
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (8) = happyShift action_3
action_10 (9) = happyShift action_4
action_10 (10) = happyShift action_5
action_10 (11) = happyShift action_6
action_10 (12) = happyShift action_7
action_10 (13) = happyShift action_8
action_10 (14) = happyShift action_9
action_10 (15) = happyShift action_10
action_10 (16) = happyShift action_11
action_10 (17) = happyShift action_12
action_10 (19) = happyShift action_13
action_10 (22) = happyShift action_14
action_10 (4) = happyGoto action_22
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (8) = happyShift action_3
action_11 (9) = happyShift action_4
action_11 (10) = happyShift action_5
action_11 (11) = happyShift action_6
action_11 (12) = happyShift action_7
action_11 (13) = happyShift action_8
action_11 (14) = happyShift action_9
action_11 (15) = happyShift action_10
action_11 (16) = happyShift action_11
action_11 (17) = happyShift action_12
action_11 (19) = happyShift action_13
action_11 (22) = happyShift action_14
action_11 (4) = happyGoto action_21
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (8) = happyShift action_3
action_12 (9) = happyShift action_4
action_12 (10) = happyShift action_5
action_12 (11) = happyShift action_6
action_12 (12) = happyShift action_7
action_12 (13) = happyShift action_8
action_12 (14) = happyShift action_9
action_12 (15) = happyShift action_10
action_12 (16) = happyShift action_11
action_12 (17) = happyShift action_12
action_12 (19) = happyShift action_13
action_12 (22) = happyShift action_14
action_12 (4) = happyGoto action_20
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (8) = happyShift action_3
action_13 (9) = happyShift action_4
action_13 (10) = happyShift action_5
action_13 (11) = happyShift action_6
action_13 (12) = happyShift action_7
action_13 (13) = happyShift action_8
action_13 (14) = happyShift action_9
action_13 (15) = happyShift action_10
action_13 (16) = happyShift action_11
action_13 (17) = happyShift action_12
action_13 (19) = happyShift action_13
action_13 (22) = happyShift action_14
action_13 (4) = happyGoto action_18
action_13 (5) = happyGoto action_19
action_13 _ = happyReduce_18

action_14 _ = happyReduce_13

action_15 (6) = happyShift action_16
action_15 (7) = happyShift action_17
action_15 (23) = happyAccept
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (8) = happyShift action_3
action_16 (9) = happyShift action_4
action_16 (10) = happyShift action_5
action_16 (11) = happyShift action_6
action_16 (12) = happyShift action_7
action_16 (13) = happyShift action_8
action_16 (14) = happyShift action_9
action_16 (15) = happyShift action_10
action_16 (16) = happyShift action_11
action_16 (17) = happyShift action_12
action_16 (19) = happyShift action_13
action_16 (22) = happyShift action_14
action_16 (4) = happyGoto action_34
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (8) = happyShift action_3
action_17 (9) = happyShift action_4
action_17 (10) = happyShift action_5
action_17 (11) = happyShift action_6
action_17 (12) = happyShift action_7
action_17 (13) = happyShift action_8
action_17 (14) = happyShift action_9
action_17 (15) = happyShift action_10
action_17 (16) = happyShift action_11
action_17 (17) = happyShift action_12
action_17 (19) = happyShift action_13
action_17 (22) = happyShift action_14
action_17 (4) = happyGoto action_33
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (6) = happyShift action_16
action_18 (7) = happyShift action_17
action_18 _ = happyReduce_17

action_19 (20) = happyShift action_31
action_19 (21) = happyShift action_32
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (6) = happyShift action_16
action_20 (7) = happyShift action_17
action_20 (18) = happyShift action_30
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_11

action_22 _ = happyReduce_10

action_23 _ = happyReduce_9

action_24 _ = happyReduce_8

action_25 _ = happyReduce_7

action_26 _ = happyReduce_6

action_27 _ = happyReduce_5

action_28 _ = happyReduce_4

action_29 _ = happyReduce_3

action_30 _ = happyReduce_12

action_31 _ = happyReduce_14

action_32 (8) = happyShift action_3
action_32 (9) = happyShift action_4
action_32 (10) = happyShift action_5
action_32 (11) = happyShift action_6
action_32 (12) = happyShift action_7
action_32 (13) = happyShift action_8
action_32 (14) = happyShift action_9
action_32 (15) = happyShift action_10
action_32 (16) = happyShift action_11
action_32 (17) = happyShift action_12
action_32 (19) = happyShift action_13
action_32 (22) = happyShift action_14
action_32 (4) = happyGoto action_35
action_32 _ = happyReduce_16

action_33 _ = happyReduce_2

action_34 (7) = happyShift action_17
action_34 _ = happyReduce_1

action_35 (6) = happyShift action_16
action_35 (7) = happyShift action_17
action_35 _ = happyReduce_15

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or (info happy_var_1 <-> info happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And (info happy_var_1 <-> info happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (Not (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (AncestorNameIs (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (AncestorNameStartsWith (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (AncestorNameEndsWith (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (AncestorNameContains (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (NameIs (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (NameStartsWith (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (NameEndsWith (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (NameContains (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (EPar (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  4 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (unTok happy_var_1 (\range (L.String str) -> EString range $ unQuote str)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (EList (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_3 : happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  5 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  5 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 ([ happy_var_1 ]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  5 happyReduction_18
happyReduction_18  =  HappyAbsSyn5
		 ([]
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken L.EOF _ -> action 23 23 tk (HappyState action) sts stk;
	L.RangedToken L.Or _ -> cont 6;
	L.RangedToken L.And _ -> cont 7;
	L.RangedToken L.Not _ -> cont 8;
	L.RangedToken L.AncestorNameIs _ -> cont 9;
	L.RangedToken L.AncestorNameStartsWith _ -> cont 10;
	L.RangedToken L.AncestorNameEndsWith _ -> cont 11;
	L.RangedToken L.AncestorNameContains _ -> cont 12;
	L.RangedToken L.NameIs _ -> cont 13;
	L.RangedToken L.NameStartsWith _ -> cont 14;
	L.RangedToken L.NameEndsWith _ -> cont 15;
	L.RangedToken L.NameContains _ -> cont 16;
	L.RangedToken L.LPar _ -> cont 17;
	L.RangedToken L.RPar _ -> cont 18;
	L.RangedToken L.LBrack _ -> cont 19;
	L.RangedToken L.RBrack _ -> cont 20;
	L.RangedToken L.Comma _ -> cont 21;
	L.RangedToken (L.String _) _ -> cont 22;
	_ -> happyError' (tk, [])
	})

happyError_ explist 23 tk = happyError' (tk, explist)
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
