import Prelude hiding (Word, getLine)

import Test.Hspec

import Data.ByteString.Lazy.Char8 (ByteString, pack, isSuffixOf)
import Debug.Trace (trace, traceShowId)
import System.FilePath
import System.Directory.Tree

import AST
import TreeFilter

testDataPath :: FilePath
testDataPath = "test/test-data"

treeA :: DirTree ()
treeA = Dir "a" [
    File "file_a_1.cpp" ()
    , File "file_a_2.hpp" ()
    ]

treeB :: DirTree ()
treeB = Dir "b" [
    File "file_b_1.cpp" ()
    , File "file_b_2.hpp" ()
    ]

treeC :: DirTree ()
treeC = Dir "c" [ ]

treeTestData :: DirTree ()
treeTestData = Dir "test-data" [ treeA, treeB, treeC ]

compareToExpected :: DirTree () -> DirTree FsObjData -> IO ()
compareToExpected expected actual =
    let actual' = () <$ actual
    in actual' `shouldBe` expected

main :: IO ()
main = hspec $ do
    describe "filterTree" $ do
        it "Correctly reproduces a tree if include is used" $ do
            let expected = treeTestData
            applyFilterWith testDataPath "asses" ( compareToExpected expected )
        it "Correctly excludes all folders but one with isChildOf" $ do
            let expected = Dir "test-data" [ treeA ]
            applyFilterWith testDataPath "isChildOf \"a\"" ( compareToExpected expected )
        it "Correctly executes nameEndsWith" $ do
            let treeA' = filterDir (\dt -> isSuffixOf ".cpp" $ pack $ name dt) treeA
            let treeB' = filterDir (\dt -> isSuffixOf ".cpp" $ pack $ name dt) treeB
            let expected = Dir "test-data" [ treeA' , treeB' ]
            applyFilterWith testDataPath "nameEndsWith \".cpp\"" ( compareToExpected expected )

