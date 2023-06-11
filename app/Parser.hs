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
	| HappyAbsSyn7 ([Exp L.Range])

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
 happyReduce_23,
 happyReduce_24 :: () => ({-HappyReduction (L.Alex) = -}
	   Prelude.Int 
	-> (L.RangedToken)
	-> HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)
	-> [HappyState (L.RangedToken) (HappyStk HappyAbsSyn -> (L.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (L.Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,113) ([64640,9535,65088,4767,192,0,0,0,0,0,0,0,65522,148,32761,32842,16380,16421,40958,8210,20479,36873,43007,51204,21503,58370,10751,61953,38143,0,0,0,0,64,0,65312,2383,65424,1191,0,0,24,0,65522,148,32761,74,3,0,0,49164,32768,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65508,297,0,0,0,32768,16380,37,0,32768,0,24576,0,12288,2048,58368,10751,1,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTreeSurgeon","exp","name","dec","listParse__exp__","identifier","'|'","'&'","'!'","ancestorNameIs","ancestorNameStartsWith","ancestorNameEndsWith","ancestorNameContains","nameIs","nameStartsWith","nameEndsWith","nameContains","all","none","let","'='","in","'('","')'","'['","']'","','","string","%eof"]
        bit_start = st Prelude.* 31
        bit_end = (st Prelude.+ 1) Prelude.* 31
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..30]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (8) = happyShift action_5
action_0 (11) = happyShift action_6
action_0 (12) = happyShift action_7
action_0 (13) = happyShift action_8
action_0 (14) = happyShift action_9
action_0 (15) = happyShift action_10
action_0 (16) = happyShift action_11
action_0 (17) = happyShift action_12
action_0 (18) = happyShift action_13
action_0 (19) = happyShift action_14
action_0 (20) = happyShift action_15
action_0 (21) = happyShift action_16
action_0 (22) = happyShift action_17
action_0 (25) = happyShift action_18
action_0 (27) = happyShift action_19
action_0 (30) = happyShift action_20
action_0 (4) = happyGoto action_21
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_5
action_1 (11) = happyShift action_6
action_1 (12) = happyShift action_7
action_1 (13) = happyShift action_8
action_1 (14) = happyShift action_9
action_1 (15) = happyShift action_10
action_1 (16) = happyShift action_11
action_1 (17) = happyShift action_12
action_1 (18) = happyShift action_13
action_1 (19) = happyShift action_14
action_1 (20) = happyShift action_15
action_1 (21) = happyShift action_16
action_1 (22) = happyShift action_17
action_1 (25) = happyShift action_18
action_1 (27) = happyShift action_19
action_1 (30) = happyShift action_20
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (9) = happyShift action_22
action_2 (10) = happyShift action_23
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_8

action_4 _ = happyReduce_18

action_5 _ = happyReduce_19

action_6 (8) = happyShift action_5
action_6 (11) = happyShift action_6
action_6 (12) = happyShift action_7
action_6 (13) = happyShift action_8
action_6 (14) = happyShift action_9
action_6 (15) = happyShift action_10
action_6 (16) = happyShift action_11
action_6 (17) = happyShift action_12
action_6 (18) = happyShift action_13
action_6 (19) = happyShift action_14
action_6 (20) = happyShift action_15
action_6 (21) = happyShift action_16
action_6 (22) = happyShift action_17
action_6 (25) = happyShift action_18
action_6 (27) = happyShift action_19
action_6 (30) = happyShift action_20
action_6 (4) = happyGoto action_36
action_6 (5) = happyGoto action_3
action_6 (6) = happyGoto action_4
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (8) = happyShift action_5
action_7 (11) = happyShift action_6
action_7 (12) = happyShift action_7
action_7 (13) = happyShift action_8
action_7 (14) = happyShift action_9
action_7 (15) = happyShift action_10
action_7 (16) = happyShift action_11
action_7 (17) = happyShift action_12
action_7 (18) = happyShift action_13
action_7 (19) = happyShift action_14
action_7 (20) = happyShift action_15
action_7 (21) = happyShift action_16
action_7 (22) = happyShift action_17
action_7 (25) = happyShift action_18
action_7 (27) = happyShift action_19
action_7 (30) = happyShift action_20
action_7 (4) = happyGoto action_35
action_7 (5) = happyGoto action_3
action_7 (6) = happyGoto action_4
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (8) = happyShift action_5
action_8 (11) = happyShift action_6
action_8 (12) = happyShift action_7
action_8 (13) = happyShift action_8
action_8 (14) = happyShift action_9
action_8 (15) = happyShift action_10
action_8 (16) = happyShift action_11
action_8 (17) = happyShift action_12
action_8 (18) = happyShift action_13
action_8 (19) = happyShift action_14
action_8 (20) = happyShift action_15
action_8 (21) = happyShift action_16
action_8 (22) = happyShift action_17
action_8 (25) = happyShift action_18
action_8 (27) = happyShift action_19
action_8 (30) = happyShift action_20
action_8 (4) = happyGoto action_34
action_8 (5) = happyGoto action_3
action_8 (6) = happyGoto action_4
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (8) = happyShift action_5
action_9 (11) = happyShift action_6
action_9 (12) = happyShift action_7
action_9 (13) = happyShift action_8
action_9 (14) = happyShift action_9
action_9 (15) = happyShift action_10
action_9 (16) = happyShift action_11
action_9 (17) = happyShift action_12
action_9 (18) = happyShift action_13
action_9 (19) = happyShift action_14
action_9 (20) = happyShift action_15
action_9 (21) = happyShift action_16
action_9 (22) = happyShift action_17
action_9 (25) = happyShift action_18
action_9 (27) = happyShift action_19
action_9 (30) = happyShift action_20
action_9 (4) = happyGoto action_33
action_9 (5) = happyGoto action_3
action_9 (6) = happyGoto action_4
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (8) = happyShift action_5
action_10 (11) = happyShift action_6
action_10 (12) = happyShift action_7
action_10 (13) = happyShift action_8
action_10 (14) = happyShift action_9
action_10 (15) = happyShift action_10
action_10 (16) = happyShift action_11
action_10 (17) = happyShift action_12
action_10 (18) = happyShift action_13
action_10 (19) = happyShift action_14
action_10 (20) = happyShift action_15
action_10 (21) = happyShift action_16
action_10 (22) = happyShift action_17
action_10 (25) = happyShift action_18
action_10 (27) = happyShift action_19
action_10 (30) = happyShift action_20
action_10 (4) = happyGoto action_32
action_10 (5) = happyGoto action_3
action_10 (6) = happyGoto action_4
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (8) = happyShift action_5
action_11 (11) = happyShift action_6
action_11 (12) = happyShift action_7
action_11 (13) = happyShift action_8
action_11 (14) = happyShift action_9
action_11 (15) = happyShift action_10
action_11 (16) = happyShift action_11
action_11 (17) = happyShift action_12
action_11 (18) = happyShift action_13
action_11 (19) = happyShift action_14
action_11 (20) = happyShift action_15
action_11 (21) = happyShift action_16
action_11 (22) = happyShift action_17
action_11 (25) = happyShift action_18
action_11 (27) = happyShift action_19
action_11 (30) = happyShift action_20
action_11 (4) = happyGoto action_31
action_11 (5) = happyGoto action_3
action_11 (6) = happyGoto action_4
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (8) = happyShift action_5
action_12 (11) = happyShift action_6
action_12 (12) = happyShift action_7
action_12 (13) = happyShift action_8
action_12 (14) = happyShift action_9
action_12 (15) = happyShift action_10
action_12 (16) = happyShift action_11
action_12 (17) = happyShift action_12
action_12 (18) = happyShift action_13
action_12 (19) = happyShift action_14
action_12 (20) = happyShift action_15
action_12 (21) = happyShift action_16
action_12 (22) = happyShift action_17
action_12 (25) = happyShift action_18
action_12 (27) = happyShift action_19
action_12 (30) = happyShift action_20
action_12 (4) = happyGoto action_30
action_12 (5) = happyGoto action_3
action_12 (6) = happyGoto action_4
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (8) = happyShift action_5
action_13 (11) = happyShift action_6
action_13 (12) = happyShift action_7
action_13 (13) = happyShift action_8
action_13 (14) = happyShift action_9
action_13 (15) = happyShift action_10
action_13 (16) = happyShift action_11
action_13 (17) = happyShift action_12
action_13 (18) = happyShift action_13
action_13 (19) = happyShift action_14
action_13 (20) = happyShift action_15
action_13 (21) = happyShift action_16
action_13 (22) = happyShift action_17
action_13 (25) = happyShift action_18
action_13 (27) = happyShift action_19
action_13 (30) = happyShift action_20
action_13 (4) = happyGoto action_29
action_13 (5) = happyGoto action_3
action_13 (6) = happyGoto action_4
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (8) = happyShift action_5
action_14 (11) = happyShift action_6
action_14 (12) = happyShift action_7
action_14 (13) = happyShift action_8
action_14 (14) = happyShift action_9
action_14 (15) = happyShift action_10
action_14 (16) = happyShift action_11
action_14 (17) = happyShift action_12
action_14 (18) = happyShift action_13
action_14 (19) = happyShift action_14
action_14 (20) = happyShift action_15
action_14 (21) = happyShift action_16
action_14 (22) = happyShift action_17
action_14 (25) = happyShift action_18
action_14 (27) = happyShift action_19
action_14 (30) = happyShift action_20
action_14 (4) = happyGoto action_28
action_14 (5) = happyGoto action_3
action_14 (6) = happyGoto action_4
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_13

action_16 _ = happyReduce_14

action_17 (8) = happyShift action_5
action_17 (5) = happyGoto action_27
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (8) = happyShift action_5
action_18 (11) = happyShift action_6
action_18 (12) = happyShift action_7
action_18 (13) = happyShift action_8
action_18 (14) = happyShift action_9
action_18 (15) = happyShift action_10
action_18 (16) = happyShift action_11
action_18 (17) = happyShift action_12
action_18 (18) = happyShift action_13
action_18 (19) = happyShift action_14
action_18 (20) = happyShift action_15
action_18 (21) = happyShift action_16
action_18 (22) = happyShift action_17
action_18 (25) = happyShift action_18
action_18 (27) = happyShift action_19
action_18 (30) = happyShift action_20
action_18 (4) = happyGoto action_26
action_18 (5) = happyGoto action_3
action_18 (6) = happyGoto action_4
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (8) = happyShift action_5
action_19 (11) = happyShift action_6
action_19 (12) = happyShift action_7
action_19 (13) = happyShift action_8
action_19 (14) = happyShift action_9
action_19 (15) = happyShift action_10
action_19 (16) = happyShift action_11
action_19 (17) = happyShift action_12
action_19 (18) = happyShift action_13
action_19 (19) = happyShift action_14
action_19 (20) = happyShift action_15
action_19 (21) = happyShift action_16
action_19 (22) = happyShift action_17
action_19 (25) = happyShift action_18
action_19 (27) = happyShift action_19
action_19 (30) = happyShift action_20
action_19 (4) = happyGoto action_24
action_19 (5) = happyGoto action_3
action_19 (6) = happyGoto action_4
action_19 (7) = happyGoto action_25
action_19 _ = happyReduce_24

action_20 _ = happyReduce_16

action_21 (9) = happyShift action_22
action_21 (10) = happyShift action_23
action_21 (31) = happyAccept
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (8) = happyShift action_5
action_22 (11) = happyShift action_6
action_22 (12) = happyShift action_7
action_22 (13) = happyShift action_8
action_22 (14) = happyShift action_9
action_22 (15) = happyShift action_10
action_22 (16) = happyShift action_11
action_22 (17) = happyShift action_12
action_22 (18) = happyShift action_13
action_22 (19) = happyShift action_14
action_22 (20) = happyShift action_15
action_22 (21) = happyShift action_16
action_22 (22) = happyShift action_17
action_22 (25) = happyShift action_18
action_22 (27) = happyShift action_19
action_22 (30) = happyShift action_20
action_22 (4) = happyGoto action_42
action_22 (5) = happyGoto action_3
action_22 (6) = happyGoto action_4
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (8) = happyShift action_5
action_23 (11) = happyShift action_6
action_23 (12) = happyShift action_7
action_23 (13) = happyShift action_8
action_23 (14) = happyShift action_9
action_23 (15) = happyShift action_10
action_23 (16) = happyShift action_11
action_23 (17) = happyShift action_12
action_23 (18) = happyShift action_13
action_23 (19) = happyShift action_14
action_23 (20) = happyShift action_15
action_23 (21) = happyShift action_16
action_23 (22) = happyShift action_17
action_23 (25) = happyShift action_18
action_23 (27) = happyShift action_19
action_23 (30) = happyShift action_20
action_23 (4) = happyGoto action_41
action_23 (5) = happyGoto action_3
action_23 (6) = happyGoto action_4
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (9) = happyShift action_22
action_24 (10) = happyShift action_23
action_24 _ = happyReduce_23

action_25 (28) = happyShift action_39
action_25 (29) = happyShift action_40
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (9) = happyShift action_22
action_26 (10) = happyShift action_23
action_26 (26) = happyShift action_38
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (23) = happyShift action_37
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_12

action_29 _ = happyReduce_11

action_30 _ = happyReduce_10

action_31 _ = happyReduce_9

action_32 _ = happyReduce_7

action_33 _ = happyReduce_6

action_34 _ = happyReduce_5

action_35 _ = happyReduce_4

action_36 _ = happyReduce_3

action_37 (8) = happyShift action_5
action_37 (11) = happyShift action_6
action_37 (12) = happyShift action_7
action_37 (13) = happyShift action_8
action_37 (14) = happyShift action_9
action_37 (15) = happyShift action_10
action_37 (16) = happyShift action_11
action_37 (17) = happyShift action_12
action_37 (18) = happyShift action_13
action_37 (19) = happyShift action_14
action_37 (20) = happyShift action_15
action_37 (21) = happyShift action_16
action_37 (22) = happyShift action_17
action_37 (25) = happyShift action_18
action_37 (27) = happyShift action_19
action_37 (30) = happyShift action_20
action_37 (4) = happyGoto action_44
action_37 (5) = happyGoto action_3
action_37 (6) = happyGoto action_4
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_15

action_39 _ = happyReduce_17

action_40 (8) = happyShift action_5
action_40 (11) = happyShift action_6
action_40 (12) = happyShift action_7
action_40 (13) = happyShift action_8
action_40 (14) = happyShift action_9
action_40 (15) = happyShift action_10
action_40 (16) = happyShift action_11
action_40 (17) = happyShift action_12
action_40 (18) = happyShift action_13
action_40 (19) = happyShift action_14
action_40 (20) = happyShift action_15
action_40 (21) = happyShift action_16
action_40 (22) = happyShift action_17
action_40 (25) = happyShift action_18
action_40 (27) = happyShift action_19
action_40 (30) = happyShift action_20
action_40 (4) = happyGoto action_43
action_40 (5) = happyGoto action_3
action_40 (6) = happyGoto action_4
action_40 _ = happyReduce_22

action_41 _ = happyReduce_2

action_42 (10) = happyShift action_23
action_42 _ = happyReduce_1

action_43 (9) = happyShift action_22
action_43 (10) = happyShift action_23
action_43 _ = happyReduce_21

action_44 (9) = happyShift action_22
action_44 (10) = happyShift action_23
action_44 (24) = happyShift action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (8) = happyShift action_5
action_45 (11) = happyShift action_6
action_45 (12) = happyShift action_7
action_45 (13) = happyShift action_8
action_45 (14) = happyShift action_9
action_45 (15) = happyShift action_10
action_45 (16) = happyShift action_11
action_45 (17) = happyShift action_12
action_45 (18) = happyShift action_13
action_45 (19) = happyShift action_14
action_45 (20) = happyShift action_15
action_45 (21) = happyShift action_16
action_45 (22) = happyShift action_17
action_45 (25) = happyShift action_18
action_45 (27) = happyShift action_19
action_45 (30) = happyShift action_20
action_45 (4) = happyGoto action_46
action_45 (5) = happyGoto action_3
action_45 (6) = happyGoto action_4
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_20

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
	(HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (EList (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  4 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  5 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (unTok happy_var_1 (\range (L.Identifier nm) -> VarName range nm)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happyReduce 6 6 happyReduction_20
happyReduction_20 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let (L.rtRange happy_var_1 <-> info happy_var_6) happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  7 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  7 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  7 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 ([ happy_var_1 ]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  7 happyReduction_24
happyReduction_24  =  HappyAbsSyn7
		 ([]
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken L.EOF _ -> action 31 31 tk (HappyState action) sts stk;
	L.RangedToken (L.Identifier _) _ -> cont 8;
	L.RangedToken L.Or _ -> cont 9;
	L.RangedToken L.And _ -> cont 10;
	L.RangedToken L.Not _ -> cont 11;
	L.RangedToken L.AncestorNameIs _ -> cont 12;
	L.RangedToken L.AncestorNameStartsWith _ -> cont 13;
	L.RangedToken L.AncestorNameEndsWith _ -> cont 14;
	L.RangedToken L.AncestorNameContains _ -> cont 15;
	L.RangedToken L.NameIs _ -> cont 16;
	L.RangedToken L.NameStartsWith _ -> cont 17;
	L.RangedToken L.NameEndsWith _ -> cont 18;
	L.RangedToken L.NameContains _ -> cont 19;
	L.RangedToken L.All _ -> cont 20;
	L.RangedToken L.None _ -> cont 21;
	L.RangedToken L.Let _ -> cont 22;
	L.RangedToken L.Eq _ -> cont 23;
	L.RangedToken L.In _ -> cont 24;
	L.RangedToken L.LPar _ -> cont 25;
	L.RangedToken L.RPar _ -> cont 26;
	L.RangedToken L.LBrack _ -> cont 27;
	L.RangedToken L.RBrack _ -> cont 28;
	L.RangedToken L.Comma _ -> cont 29;
	L.RangedToken (L.String _) _ -> cont 30;
	_ -> happyError' (tk, [])
	})

happyError_ explist 31 tk = happyError' (tk, explist)
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
