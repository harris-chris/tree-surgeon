import Prelude hiding (Word, getLine)

import Test.Hspec

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Data.Maybe
import Debug.Trace (trace, traceShowId)
import System.FilePath
import System.Directory.Tree

import AST
import Output
import TreeFilter

testDataPath :: FilePath
testDataPath = "test/test-data"

treeA :: DirTree ()
treeA = Dir "a-project" [
    File "file_a_1.cpp" ()
    , File "file_a_2.hpp" ()
    , Dir "docs" [
        File "docs.md" ()
        ]
    ]

treeB :: DirTree ()
treeB = Dir "b-library" [
    File "file_b_1.cpp" ()
    , File "file_b_2.hpp" ()
    ]

treeC :: DirTree ()
treeC = Dir "c-executable" [
    File "file_c_1.hs" ()
    , Dir "ext" [
        File "binary" ()
        ]
    ]

filterOutDirs :: DirTree () -> DirTree ()
filterOutDirs (Dir name contents) = Dir name (catMaybes $ filterContents <$> contents)
    where filterContents (Dir _ _) = Nothing
          filterContents f = Just f
filterOutDirs _ = error "Can only filter directory"

treeTestData :: DirTree ()
treeTestData = Dir "test-data" [ treeA, treeB, treeC ]

compareToExpected :: DirTree () -> DirTree a -> IO ()
compareToExpected expected actual =
    let actual' = () <$ actual
    in actual' `shouldBe` expected

main :: IO ()
main = hspec $ do
    describe "Atomic expressions" $ do
        it "Correctly executes ancestorNameIs string" $ do
            let expected = Dir "test-data" [ treeA ]
            let testStr = "ancestorNameIs \"a-project\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameIs [string]" $ do
            let expected = Dir "test-data" [ treeA, treeB ]
            let testStr = "ancestorNameIs [\"a-project\", \"b-library\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameStartsWith string" $ do
            let expected = Dir "test-data" [ treeA ]
            let testStr = "ancestorNameStartsWith \"a-proj\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameStartsWith [string]" $ do
            let expected = Dir "test-data" [ treeA, treeB ]
            let testStr = "ancestorNameStartsWith [\"a-proj\", \"b-library\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameEndsWith string" $ do
            let expected = Dir "test-data" [ treeA ]
            let testStr = "ancestorNameEndsWith \"project\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameEndsWith [string]" $ do
            let expected = Dir "test-data" [ treeA, treeB ]
            let testStr = "ancestorNameEndsWith [\"project\", \"library\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameContains string" $ do
            let expected = Dir "test-data" [ treeB ]
            let testStr = "ancestorNameContains \"lib\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameContains [string]" $ do
            let expected = Dir "test-data" [ treeA , treeB ]
            let testStr = "ancestorNameContains [\"lib\",\"proj\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes nameIs string" $ do
            let treeA' = filterDir (\dt -> (name dt) == "file_a_2.hpp" ) treeA
            let expected = Dir "test-data" [ treeA' ]
            let testStr = "nameIs \"file_a_2.hpp\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes nameIs [string]" $ do
            let treeA' = filterDir (\dt -> (name dt) == "file_a_2.hpp" ) treeA
            let treeB' = filterDir (\dt -> (name dt) == "file_b_1.cpp" ) treeB
            let expected = Dir "test-data" [ treeA' , treeB' ]
            let testStr = "nameIs [\"file_a_2.hpp\", \"file_b_1.cpp\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes nameStartsWith [string]" $ do
            let treeA' = filterDir (\dt -> LBS.isPrefixOf "docs" (LBS.pack $ name dt)) treeA
            let expected = Dir "test-data" [ treeA' ]
            let testStr = "nameStartsWith \"docs\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes nameStartsWith [string]" $ do
            let treeA' = filterDir (\dt -> LBS.isPrefixOf "docs" (LBS.pack $ name dt)) treeA
            let treeC' = filterDir
                          (\dt -> LBS.isPrefixOf "ext" (LBS.pack $ name dt)
                          || LBS.isPrefixOf "bin" (LBS.pack $ name dt)
                          ) treeC
            let expected = Dir "test-data" [ treeA' , treeC' ]
            let testStr = "nameStartsWith [\"docs\", \"bin\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes nameEndsWith string" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
            let expected = Dir "test-data" [ treeA' , treeB' ]
            let testStr = "nameEndsWith \".cpp\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes nameEndsWith [string]" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
            let treeC' = filterDir (\dt -> LBS.isSuffixOf ".hs" $ LBS.pack $ name dt) treeC
            let expected = Dir "test-data" [ treeA' , treeB' , treeC' ]
            let testStr = "nameEndsWith [\".cpp\", \".hs\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes nameContains string" $ do
            let treeB' = filterDir (\dt -> BS.isInfixOf "file_b" (BS.pack $ name dt)) treeB
            let expected = Dir "test-data" [ treeB' ]
            let testStr = "nameContains \"file_b\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
    describe "Combinations of expressions" $ do
        it "Correctly executes exp | exp" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let expected = Dir "test-data" [ treeA' , treeB ]
            let testStr = "nameEndsWith \".cpp\" | ancestorNameIs \"b-library\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ( exp )" $ do
            let expected = Dir "test-data" [ treeA ]
            let testStr = "(ancestorNameIs \"a-project\")"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ( exp | exp )" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let expected = Dir "test-data" [ treeA' , treeB ]
            let testStr = "( nameEndsWith \".cpp\" | ancestorNameIs \"b-library\" )"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ( exp | exp | exp )" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let treeC' = filterOutDirs treeC
            let expected = Dir "test-data" [ treeA' , treeB , treeC' ]
            let testStr = "( nameEndsWith \".cpp\" | ancestorNameIs \"b-library\" | nameEndsWith \".hs\" )"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes exp & exp" $ do
            let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
            let expected = Dir "test-data" [ treeB' ]
            let testStr = "nameEndsWith \".cpp\" & ancestorNameIs \"b-library\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ( exp | exp ) &  exp" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
            let expected = Dir "test-data" [ treeA' , treeB' ]
            let testStr = "( nameEndsWith \".cpp\" & ( ancestorNameIs \"a-project\" | ancestorNameIs \"b-library\" ) )"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ( exp & exp ) | ( exp & exp )" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let treeB' = filterDir (\dt -> LBS.isSuffixOf ".hpp" $ LBS.pack $ name dt) treeB
            let expected = Dir "test-data" [ treeA' , treeB' ]
            let testStr = "( nameEndsWith \".cpp\" & ancestorNameIs \"a-project\" ) | ( nameEndsWith \".hpp\" & ancestorNameIs \"b-library\" )"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        -- it "Errors if a bad string is passed" $ do
        --     let testStr = "isNotAToken is not a valid expression"
        --     applyFilterWith testDataPath testStr ( putStrLn . show ) `shouldThrow`
        --         (== Couldn'tLex (show testStr))
    describe "Bash array functions" $ do
        it "Correctly includes" $ do
            let testStr = "nameEndsWith [\"cpp\"]"
            let expected = (normalise . ("test-data" </>)) <$>
                    [ "a-project/file_a_1.cpp"
                    , "b-library/file_b_1.cpp"
                    , "a-project"
                    , "b-library"
                    , "" ]
            let compFunc = \f -> (sort $ toBashArray f) `shouldBe` (sort expected)
            applyFilterWith testDataPath compFunc testStr
        it "Correctly excludes" $ do
            let testStr = "nameIs [\"docs.md\", \"binary\"] | nameEndsWith \".hs\""
            let expected = (normalise . ("test-data" </>)) <$>
                    [ "a-project/file_a_1.cpp"
                    , "a-project/file_a_2.hpp"
                    , "b-library/file_b_1.cpp"
                    , "b-library/file_b_2.hpp"
                    , "b-library" ]
            let compFunc = \o f -> (sort $ getExcluded o f) `shouldBe` (sort expected)
            applyFilterWithComparative testDataPath compFunc testStr

