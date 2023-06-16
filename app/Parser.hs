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
	| HappyAbsSyn5 (VarName L.Range)
	| HappyAbsSyn6 (NamedExpr L.Range)
	| HappyAbsSyn7 ([NamedExpr L.Range])
	| HappyAbsSyn8 ([Exp L.Range])

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
 action_46,
 action_47,
 action_48,
 action_49 :: () => Prelude.Int -> ({-HappyReduction (L.Alex) = -}
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
 happyReduce_26 :: () => ({-HappyReduction (L.Alex) = -}
	   Prelude.Int 
	-> (L.RangedToken)
	-> HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)
	-> [HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (L.Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,113) ([63744,19071,63744,19071,1536,0,0,0,0,0,63744,19071,63744,19071,63744,19071,63744,19071,63744,19071,63744,19071,63744,19071,63744,19071,63744,19071,0,0,0,0,256,0,63744,19071,0,0,0,0,1536,0,63744,19071,63744,19071,1536,0,63744,23167,1536,1024,0,128,0,8192,0,8448,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63744,19071,256,0,0,0,63744,19071,0,0,1536,0,0,0,0,0,1024,0,1536,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTreeSurgeon","exp","name","namedExpr","decListParse__namedExpr__","listParse__exp__","identifier","'|'","'&'","'!'","ancestorNameIs","ancestorNameStartsWith","ancestorNameEndsWith","ancestorNameContains","nameIs","nameStartsWith","nameEndsWith","nameContains","all","none","let","'='","in","'('","')'","'['","']'","';'","string","%eof"]
        bit_start = st Prelude.* 32
        bit_end = (st Prelude.+ 1) Prelude.* 32
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..31]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (9) = happyShift action_4
action_0 (12) = happyShift action_5
action_0 (13) = happyShift action_6
action_0 (14) = happyShift action_7
action_0 (15) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (17) = happyShift action_10
action_0 (18) = happyShift action_11
action_0 (19) = happyShift action_12
action_0 (20) = happyShift action_13
action_0 (21) = happyShift action_14
action_0 (22) = happyShift action_15
action_0 (23) = happyShift action_16
action_0 (26) = happyShift action_17
action_0 (28) = happyShift action_18
action_0 (31) = happyShift action_19
action_0 (4) = happyGoto action_20
action_0 (5) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (9) = happyShift action_4
action_1 (12) = happyShift action_5
action_1 (13) = happyShift action_6
action_1 (14) = happyShift action_7
action_1 (15) = happyShift action_8
action_1 (16) = happyShift action_9
action_1 (17) = happyShift action_10
action_1 (18) = happyShift action_11
action_1 (19) = happyShift action_12
action_1 (20) = happyShift action_13
action_1 (21) = happyShift action_14
action_1 (22) = happyShift action_15
action_1 (23) = happyShift action_16
action_1 (26) = happyShift action_17
action_1 (28) = happyShift action_18
action_1 (31) = happyShift action_19
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (10) = happyShift action_21
action_2 (11) = happyShift action_22
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_8

action_4 _ = happyReduce_19

action_5 (9) = happyShift action_4
action_5 (12) = happyShift action_5
action_5 (13) = happyShift action_6
action_5 (14) = happyShift action_7
action_5 (15) = happyShift action_8
action_5 (16) = happyShift action_9
action_5 (17) = happyShift action_10
action_5 (18) = happyShift action_11
action_5 (19) = happyShift action_12
action_5 (20) = happyShift action_13
action_5 (21) = happyShift action_14
action_5 (22) = happyShift action_15
action_5 (23) = happyShift action_16
action_5 (26) = happyShift action_17
action_5 (28) = happyShift action_18
action_5 (31) = happyShift action_19
action_5 (4) = happyGoto action_37
action_5 (5) = happyGoto action_3
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (9) = happyShift action_4
action_6 (12) = happyShift action_5
action_6 (13) = happyShift action_6
action_6 (14) = happyShift action_7
action_6 (15) = happyShift action_8
action_6 (16) = happyShift action_9
action_6 (17) = happyShift action_10
action_6 (18) = happyShift action_11
action_6 (19) = happyShift action_12
action_6 (20) = happyShift action_13
action_6 (21) = happyShift action_14
action_6 (22) = happyShift action_15
action_6 (23) = happyShift action_16
action_6 (26) = happyShift action_17
action_6 (28) = happyShift action_18
action_6 (31) = happyShift action_19
action_6 (4) = happyGoto action_36
action_6 (5) = happyGoto action_3
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (9) = happyShift action_4
action_7 (12) = happyShift action_5
action_7 (13) = happyShift action_6
action_7 (14) = happyShift action_7
action_7 (15) = happyShift action_8
action_7 (16) = happyShift action_9
action_7 (17) = happyShift action_10
action_7 (18) = happyShift action_11
action_7 (19) = happyShift action_12
action_7 (20) = happyShift action_13
action_7 (21) = happyShift action_14
action_7 (22) = happyShift action_15
action_7 (23) = happyShift action_16
action_7 (26) = happyShift action_17
action_7 (28) = happyShift action_18
action_7 (31) = happyShift action_19
action_7 (4) = happyGoto action_35
action_7 (5) = happyGoto action_3
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (9) = happyShift action_4
action_8 (12) = happyShift action_5
action_8 (13) = happyShift action_6
action_8 (14) = happyShift action_7
action_8 (15) = happyShift action_8
action_8 (16) = happyShift action_9
action_8 (17) = happyShift action_10
action_8 (18) = happyShift action_11
action_8 (19) = happyShift action_12
action_8 (20) = happyShift action_13
action_8 (21) = happyShift action_14
action_8 (22) = happyShift action_15
action_8 (23) = happyShift action_16
action_8 (26) = happyShift action_17
action_8 (28) = happyShift action_18
action_8 (31) = happyShift action_19
action_8 (4) = happyGoto action_34
action_8 (5) = happyGoto action_3
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (9) = happyShift action_4
action_9 (12) = happyShift action_5
action_9 (13) = happyShift action_6
action_9 (14) = happyShift action_7
action_9 (15) = happyShift action_8
action_9 (16) = happyShift action_9
action_9 (17) = happyShift action_10
action_9 (18) = happyShift action_11
action_9 (19) = happyShift action_12
action_9 (20) = happyShift action_13
action_9 (21) = happyShift action_14
action_9 (22) = happyShift action_15
action_9 (23) = happyShift action_16
action_9 (26) = happyShift action_17
action_9 (28) = happyShift action_18
action_9 (31) = happyShift action_19
action_9 (4) = happyGoto action_33
action_9 (5) = happyGoto action_3
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (9) = happyShift action_4
action_10 (12) = happyShift action_5
action_10 (13) = happyShift action_6
action_10 (14) = happyShift action_7
action_10 (15) = happyShift action_8
action_10 (16) = happyShift action_9
action_10 (17) = happyShift action_10
action_10 (18) = happyShift action_11
action_10 (19) = happyShift action_12
action_10 (20) = happyShift action_13
action_10 (21) = happyShift action_14
action_10 (22) = happyShift action_15
action_10 (23) = happyShift action_16
action_10 (26) = happyShift action_17
action_10 (28) = happyShift action_18
action_10 (31) = happyShift action_19
action_10 (4) = happyGoto action_32
action_10 (5) = happyGoto action_3
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (9) = happyShift action_4
action_11 (12) = happyShift action_5
action_11 (13) = happyShift action_6
action_11 (14) = happyShift action_7
action_11 (15) = happyShift action_8
action_11 (16) = happyShift action_9
action_11 (17) = happyShift action_10
action_11 (18) = happyShift action_11
action_11 (19) = happyShift action_12
action_11 (20) = happyShift action_13
action_11 (21) = happyShift action_14
action_11 (22) = happyShift action_15
action_11 (23) = happyShift action_16
action_11 (26) = happyShift action_17
action_11 (28) = happyShift action_18
action_11 (31) = happyShift action_19
action_11 (4) = happyGoto action_31
action_11 (5) = happyGoto action_3
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (9) = happyShift action_4
action_12 (12) = happyShift action_5
action_12 (13) = happyShift action_6
action_12 (14) = happyShift action_7
action_12 (15) = happyShift action_8
action_12 (16) = happyShift action_9
action_12 (17) = happyShift action_10
action_12 (18) = happyShift action_11
action_12 (19) = happyShift action_12
action_12 (20) = happyShift action_13
action_12 (21) = happyShift action_14
action_12 (22) = happyShift action_15
action_12 (23) = happyShift action_16
action_12 (26) = happyShift action_17
action_12 (28) = happyShift action_18
action_12 (31) = happyShift action_19
action_12 (4) = happyGoto action_30
action_12 (5) = happyGoto action_3
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (9) = happyShift action_4
action_13 (12) = happyShift action_5
action_13 (13) = happyShift action_6
action_13 (14) = happyShift action_7
action_13 (15) = happyShift action_8
action_13 (16) = happyShift action_9
action_13 (17) = happyShift action_10
action_13 (18) = happyShift action_11
action_13 (19) = happyShift action_12
action_13 (20) = happyShift action_13
action_13 (21) = happyShift action_14
action_13 (22) = happyShift action_15
action_13 (23) = happyShift action_16
action_13 (26) = happyShift action_17
action_13 (28) = happyShift action_18
action_13 (31) = happyShift action_19
action_13 (4) = happyGoto action_29
action_13 (5) = happyGoto action_3
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_13

action_15 _ = happyReduce_14

action_16 (9) = happyShift action_4
action_16 (5) = happyGoto action_26
action_16 (6) = happyGoto action_27
action_16 (7) = happyGoto action_28
action_16 _ = happyReduce_23

action_17 (9) = happyShift action_4
action_17 (12) = happyShift action_5
action_17 (13) = happyShift action_6
action_17 (14) = happyShift action_7
action_17 (15) = happyShift action_8
action_17 (16) = happyShift action_9
action_17 (17) = happyShift action_10
action_17 (18) = happyShift action_11
action_17 (19) = happyShift action_12
action_17 (20) = happyShift action_13
action_17 (21) = happyShift action_14
action_17 (22) = happyShift action_15
action_17 (23) = happyShift action_16
action_17 (26) = happyShift action_17
action_17 (28) = happyShift action_18
action_17 (31) = happyShift action_19
action_17 (4) = happyGoto action_25
action_17 (5) = happyGoto action_3
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (9) = happyShift action_4
action_18 (12) = happyShift action_5
action_18 (13) = happyShift action_6
action_18 (14) = happyShift action_7
action_18 (15) = happyShift action_8
action_18 (16) = happyShift action_9
action_18 (17) = happyShift action_10
action_18 (18) = happyShift action_11
action_18 (19) = happyShift action_12
action_18 (20) = happyShift action_13
action_18 (21) = happyShift action_14
action_18 (22) = happyShift action_15
action_18 (23) = happyShift action_16
action_18 (26) = happyShift action_17
action_18 (28) = happyShift action_18
action_18 (31) = happyShift action_19
action_18 (4) = happyGoto action_23
action_18 (5) = happyGoto action_3
action_18 (8) = happyGoto action_24
action_18 _ = happyReduce_26

action_19 _ = happyReduce_16

action_20 (10) = happyShift action_21
action_20 (11) = happyShift action_22
action_20 (32) = happyAccept
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (9) = happyShift action_4
action_21 (12) = happyShift action_5
action_21 (13) = happyShift action_6
action_21 (14) = happyShift action_7
action_21 (15) = happyShift action_8
action_21 (16) = happyShift action_9
action_21 (17) = happyShift action_10
action_21 (18) = happyShift action_11
action_21 (19) = happyShift action_12
action_21 (20) = happyShift action_13
action_21 (21) = happyShift action_14
action_21 (22) = happyShift action_15
action_21 (23) = happyShift action_16
action_21 (26) = happyShift action_17
action_21 (28) = happyShift action_18
action_21 (31) = happyShift action_19
action_21 (4) = happyGoto action_46
action_21 (5) = happyGoto action_3
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (9) = happyShift action_4
action_22 (12) = happyShift action_5
action_22 (13) = happyShift action_6
action_22 (14) = happyShift action_7
action_22 (15) = happyShift action_8
action_22 (16) = happyShift action_9
action_22 (17) = happyShift action_10
action_22 (18) = happyShift action_11
action_22 (19) = happyShift action_12
action_22 (20) = happyShift action_13
action_22 (21) = happyShift action_14
action_22 (22) = happyShift action_15
action_22 (23) = happyShift action_16
action_22 (26) = happyShift action_17
action_22 (28) = happyShift action_18
action_22 (31) = happyShift action_19
action_22 (4) = happyGoto action_45
action_22 (5) = happyGoto action_3
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (10) = happyShift action_21
action_23 (11) = happyShift action_22
action_23 _ = happyReduce_25

action_24 (9) = happyShift action_4
action_24 (12) = happyShift action_5
action_24 (13) = happyShift action_6
action_24 (14) = happyShift action_7
action_24 (15) = happyShift action_8
action_24 (16) = happyShift action_9
action_24 (17) = happyShift action_10
action_24 (18) = happyShift action_11
action_24 (19) = happyShift action_12
action_24 (20) = happyShift action_13
action_24 (21) = happyShift action_14
action_24 (22) = happyShift action_15
action_24 (23) = happyShift action_16
action_24 (26) = happyShift action_17
action_24 (28) = happyShift action_18
action_24 (29) = happyShift action_44
action_24 (31) = happyShift action_19
action_24 (4) = happyGoto action_43
action_24 (5) = happyGoto action_3
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (10) = happyShift action_21
action_25 (11) = happyShift action_22
action_25 (27) = happyShift action_42
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (24) = happyShift action_41
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (30) = happyShift action_40
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (25) = happyShift action_38
action_28 (30) = happyShift action_39
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_12

action_30 _ = happyReduce_11

action_31 _ = happyReduce_10

action_32 _ = happyReduce_9

action_33 _ = happyReduce_7

action_34 _ = happyReduce_6

action_35 _ = happyReduce_5

action_36 _ = happyReduce_4

action_37 _ = happyReduce_3

action_38 (9) = happyShift action_4
action_38 (12) = happyShift action_5
action_38 (13) = happyShift action_6
action_38 (14) = happyShift action_7
action_38 (15) = happyShift action_8
action_38 (16) = happyShift action_9
action_38 (17) = happyShift action_10
action_38 (18) = happyShift action_11
action_38 (19) = happyShift action_12
action_38 (20) = happyShift action_13
action_38 (21) = happyShift action_14
action_38 (22) = happyShift action_15
action_38 (23) = happyShift action_16
action_38 (26) = happyShift action_17
action_38 (28) = happyShift action_18
action_38 (31) = happyShift action_19
action_38 (4) = happyGoto action_49
action_38 (5) = happyGoto action_3
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (9) = happyShift action_4
action_39 (5) = happyGoto action_26
action_39 (6) = happyGoto action_48
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_22

action_41 (9) = happyShift action_4
action_41 (12) = happyShift action_5
action_41 (13) = happyShift action_6
action_41 (14) = happyShift action_7
action_41 (15) = happyShift action_8
action_41 (16) = happyShift action_9
action_41 (17) = happyShift action_10
action_41 (18) = happyShift action_11
action_41 (19) = happyShift action_12
action_41 (20) = happyShift action_13
action_41 (21) = happyShift action_14
action_41 (22) = happyShift action_15
action_41 (23) = happyShift action_16
action_41 (26) = happyShift action_17
action_41 (28) = happyShift action_18
action_41 (31) = happyShift action_19
action_41 (4) = happyGoto action_47
action_41 (5) = happyGoto action_3
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_15

action_43 (10) = happyShift action_21
action_43 (11) = happyShift action_22
action_43 _ = happyReduce_24

action_44 _ = happyReduce_17

action_45 _ = happyReduce_2

action_46 (11) = happyShift action_22
action_46 _ = happyReduce_1

action_47 (10) = happyShift action_21
action_47 (11) = happyShift action_22
action_47 _ = happyReduce_20

action_48 _ = happyReduce_21

action_49 _ = happyReduce_18

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

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (EVar (info happy_var_1) happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (NameIs (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (NameStartsWith (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (NameEndsWith (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (NameContains (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  4 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (All (L.rtRange happy_var_1 <-> L.rtRange happy_var_1)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  4 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (None (L.rtRange happy_var_1 <-> L.rtRange happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  4 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (EPar (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  4 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (unTok happy_var_1 (\range (L.String str) -> EString range $ unQuote str)
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  4 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (EList (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 4 happyReduction_18
happyReduction_18 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  5 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (unTok happy_var_1 (\range (L.Identifier nm) -> VarName range nm)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ((happy_var_1, happy_var_3)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  7 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  7 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 ([ happy_var_1 ]
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  7 happyReduction_23
happyReduction_23  =  HappyAbsSyn7
		 ([]
	)

happyReduce_24 = happySpecReduce_2  8 happyReduction_24
happyReduction_24 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  8 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ([ happy_var_1 ]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  8 happyReduction_26
happyReduction_26  =  HappyAbsSyn8
		 ([]
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken L.EOF _ -> action 32 32 tk (HappyState action) sts stk;
	L.RangedToken (L.Identifier _) _ -> cont 9;
	L.RangedToken L.Or _ -> cont 10;
	L.RangedToken L.And _ -> cont 11;
	L.RangedToken L.Not _ -> cont 12;
	L.RangedToken L.AncestorNameIs _ -> cont 13;
	L.RangedToken L.AncestorNameStartsWith _ -> cont 14;
	L.RangedToken L.AncestorNameEndsWith _ -> cont 15;
	L.RangedToken L.AncestorNameContains _ -> cont 16;
	L.RangedToken L.NameIs _ -> cont 17;
	L.RangedToken L.NameStartsWith _ -> cont 18;
	L.RangedToken L.NameEndsWith _ -> cont 19;
	L.RangedToken L.NameContains _ -> cont 20;
	L.RangedToken L.All _ -> cont 21;
	L.RangedToken L.None _ -> cont 22;
	L.RangedToken L.Let _ -> cont 23;
	L.RangedToken L.Eq _ -> cont 24;
	L.RangedToken L.In _ -> cont 25;
	L.RangedToken L.LPar _ -> cont 26;
	L.RangedToken L.RPar _ -> cont 27;
	L.RangedToken L.LBrack _ -> cont 28;
	L.RangedToken L.RBrack _ -> cont 29;
	L.RangedToken L.SemiColon _ -> cont 30;
	L.RangedToken (L.String _) _ -> cont 31;
	_ -> happyError' (tk, [])
	})

happyError_ explist 32 tk = happyError' (tk, explist)
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
