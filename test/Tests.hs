import Prelude hiding (Word, getLine)

import Test.Hspec

import Control.Exception
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
    , File "file_b_3.tmp" ()
    , Dir ".cache" [
            File "temp_b_3.tmp" ()
        ]
    ]

treeC :: DirTree ()
treeC = Dir "c-executable" [
    File "file_c_1.hs" ()
    , Dir "ext" [
        File "tree" ()
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

flatFilesOnly :: DirTree a -> [DirTree a]
flatFilesOnly tree =
    filter f $ flattenDir tree
        where f (File _ _) = True
              f (Dir _ _) = False
              f (Failed _ _) = True

{-|
  Applies two filters to the same tree, and a function which compares the resultant
  two filtered trees to the original tree. Mostly useful for completeness checks, ie
  ensuring that what has been filtered out by one string is the inverse of what has
  been filtered out by that string's inverse.
-}
applyTwoFiltersAndCompare :: FileName -> LBS.ByteString -> LBS.ByteString -> (DirTree FsObjData -> DirTree FsObjData -> DirTree FsObjData -> IO()) -> IO ()
applyTwoFiltersAndCompare dirname filtA filtB compareF =
    do
        origTree <- (toElements . dirTree) <$> readDirectoryWith return dirname
        let filteredTreeAE = filterTreeWith origTree filtA
        let filteredTreeBE = filterTreeWith origTree filtB
        case (filteredTreeAE, filteredTreeBE) of
            (Left err, _) -> throw $ err
            (_, Left err) -> throw $ err
            (Right filteredA, Right filteredB) -> compareF origTree filteredA filteredB

{-|
  Takes a single tree and single filter, and then compares the original tree (expressed
  as a bash array), the filtered tree, and the --exclude filtered tree. Mostly useful
  for completeness checks, ie to ensure that what has been excluded + what has been
  included equals the original.
-}
compareInclExcl :: FileName -> LBS.ByteString -> ([String] -> [String] -> [String] -> IO()) -> IO ()
compareInclExcl dirname filtStr compareF =
    do
        origTree <- (toElements . dirTree) <$> readDirectoryWith return dirname
        let origArray = toBashArray True origTree
        let inclArrayE = toBashArray True <$> filterTreeWith origTree filtStr
        let exclArrayE = getExcluded True origTree <$> filterTreeWith origTree filtStr
        case (inclArrayE, exclArrayE) of
            (Left err, _) -> throw $ err
            (_, Left err) -> throw $ err
            (Right inclArray, Right exclArray) -> compareF origArray inclArray exclArray

main :: IO ()
main = hspec $ do
    describe "Atomic expressions" $ do
        it "Correctly executes ancestorNameIs string" $ do
            let expected = Dir "test-data" [ treeA ]
            let testStr = "ancestorNameIs \"a-project\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameIs [string]" $ do
            let expected = Dir "test-data" [ treeA, treeB ]
            let testStr = "ancestorNameIs [\"a-project\" \"b-library\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameStartsWith string" $ do
            let expected = Dir "test-data" [ treeA ]
            let testStr = "ancestorNameStartsWith \"a-proj\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameStartsWith [string]" $ do
            let expected = Dir "test-data" [ treeA, treeB ]
            let testStr = "ancestorNameStartsWith [\"a-proj\" \"b-library\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameEndsWith string" $ do
            let expected = Dir "test-data" [ treeA ]
            let testStr = "ancestorNameEndsWith \"project\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameEndsWith [string]" $ do
            let expected = Dir "test-data" [ treeA, treeB ]
            let testStr = "ancestorNameEndsWith [\"project\" \"library\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameContains string" $ do
            let expected = Dir "test-data" [ treeB ]
            let testStr = "ancestorNameContains \"lib\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes ancestorNameContains [string]" $ do
            let expected = Dir "test-data" [ treeA , treeB ]
            let testStr = "ancestorNameContains [\"lib\" \"proj\"]"
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
            let testStr = "nameIs [\"file_a_2.hpp\" \"file_b_1.cpp\"]"
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
                          || LBS.isPrefixOf "tr" (LBS.pack $ name dt)
                          ) treeC
            let expected = Dir "test-data" [ treeA' , treeC' ]
            let testStr = "nameStartsWith [\"docs\" \"tr\"]"
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
            let testStr = "nameEndsWith [\".cpp\" \".hs\"]"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes nameContains string" $ do
            let treeB' = filterDir (\dt -> BS.isInfixOf "file_b" (BS.pack $ name dt)) treeB
            let expected = Dir "test-data" [ treeB' ]
            let testStr = "nameContains \"file_b\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes all" $ do
            let expected = Dir "test-data" [ treeA , treeB , treeC ]
            let testStr = "all"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes none" $ do
            let expected = Dir "test-data" [ ]
            let testStr = "none"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr

    describe "Inverting expressions" $ do
        it "Correctly executes !exp" $ do
            let expected = Dir "test-data" [ treeB , treeC ]
            let testStr = "!ancestorNameIs \"a-project\""
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes !(exp | exp | exp)" $ do
            let treeA' = filterDir
                          (\dt -> (not $ LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt)
                          && (not $ LBS.isSuffixOf ".hpp" $ LBS.pack $ name dt)) treeA
            let expected = Dir "test-data" [ treeA' , treeC ]
            let testStr = "!(nameEndsWith \".cpp\" | nameEndsWith \".hpp\" | nameEndsWith \".tmp\")"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Resolves the inverse of an expression + the expression to be the total" $ do
            let str = "nameEndsWith \".cpp\""
            let notStr = "!" <> str
            let compareF = \orig a b ->
                            (sort $ (flatFilesOnly a) ++ (flatFilesOnly b))
                            `shouldBe` sort (flatFilesOnly orig)
            applyTwoFiltersAndCompare testDataPath str notStr compareF

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

    describe "Let expressions" $ do
        it "Correctly executes let {matcher} in {matcher}" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
            let expected = Dir "test-data" [ treeA' , treeB' ]
            let testStr = "let isCpp = (nameEndsWith \".cpp\"); in isCpp"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes let {matcher} in ({matcher} & exp)" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let expected = Dir "test-data" [ treeA' ]
            let testStr = "let isCpp = nameEndsWith \".cpp\";"
                          <> " in ( isCpp & nameStartsWith \"file_a\")"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr
        it "Correctly executes let {matcher1} in let {matcher2} in ({matcher1} & {matcher2})" $ do
            let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
            let expected = Dir "test-data" [ treeA' ]
            let testStr = "let isCpp = nameEndsWith \".cpp\";"
                          <> " in let isFileA = nameStartsWith \"file_a\""
                          <> " in (isCpp & isFileA)"
            applyFilterWith testDataPath ( compareToExpected expected ) testStr

    describe "Bash array functions" $ do
        it "Correctly includes" $ do
            let testStr = "nameEndsWith [\"cpp\"]"
            let expected = (normalise . ("test-data" </>)) <$>
                    [ "a-project/file_a_1.cpp"
                    , "b-library/file_b_1.cpp"
                    , "a-project"
                    , "b-library"
                    , "" ]
            let compFunc = \f -> (sort $ toBashArray True f) `shouldBe` (sort expected)
            applyFilterWith testDataPath compFunc testStr
        it "Correctly excludes" $ do
            let testStr = "nameIs [\"docs.md\" \"tree\"] | nameEndsWith \".hs\""
            let expected = (normalise . ("test-data" </>)) <$>
                    [ "a-project/file_a_1.cpp"
                    , "a-project/file_a_2.hpp"
                    , "b-library/file_b_1.cpp"
                    , "b-library/file_b_2.hpp"
                    , "b-library/file_b_3.tmp"
                    , "b-library/.cache"
                    , "b-library/.cache/temp_b_3.tmp"
                    , "b-library" ]
            let compFunc = \o f -> (sort $ getExcluded True o f) `shouldBe` (sort expected)
            applyFilterWithComparative testDataPath compFunc testStr
        it "Includes plus Excludeds equals total" $ do
            let testStr = "nameIs [\"docs.md\" \"tree\"] | nameEndsWith \".hs\""
            let compareF = \orig incl excl -> (sort $ incl ++ excl) `shouldBe` (sort orig)
            compareInclExcl testDataPath testStr compareF

