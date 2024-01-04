import Prelude hiding (Word, getLine)

import Test.Hspec
import Test.HUnit.Lang

import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List
import Data.Maybe
import System.Directory (createDirectoryIfMissing)
import System.Directory.Tree
import System.FilePath (normalise, (</>))

import Cli
import Exceptions
import FData
import Lexer as X
import Output (toBashArray)
import ResolvedType
import TreeFilter

testDataPath :: FilePath
testDataPath = "test/test-data"

docsTestDataPath :: FilePath
docsTestDataPath = "test/docs-test-data"

dummyRange :: X.Range
dummyRange = X.Range (X.AlexPn 0 0 0) (X.AlexPn 0 0 0)

docsDir =
    Dir "docs" [
        File "docs.md" ()
        ]

mySrcDir =
    Dir "mySrc" [
        File "myMain.rs" ()
        ]

testsDir =
    Dir "tests" [
        Dir "integration_tests" [
            File "tests.rs" ()
            ]
        ]

getTestsDir :: [DirTree()] -> DirTree ()
getTestsDir subDirs =
    Dir "docs-test-data" [
        Dir "myDir" subDirs
    ]

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
    Dir "emptydir" []
    , File "file_c_1.hs" ()
    , Dir "ext" [
        File "tree" ()
        ]
    ]

treeTestData :: DirTree ()
treeTestData = Dir "test-data" [ treeA, treeB, treeC ]

compareToExpected :: DirTree () -> DirTree a -> IO ()
compareToExpected expected actual =
    let actual' = () <$ actual
    in actual' `shouldBe` expected

returnsExceptionOfType :: LBS.ByteString -> (TSException X.Range) -> DirTree a -> IO ()
returnsExceptionOfType filterStr expected tree =
    let actual = filterTreeWith filterStr $ toFData True [] tree
    in case (actual, expected) of
        (Right _, _) -> assertFailure "Did not return an exception"
        (Left actuals, expr) -> returnsExceptionOfType' expr actuals

returnsExceptionOfType' :: (TSException a) -> [(TSException a)] -> IO ()
returnsExceptionOfType' expected actuals =
    let matches = catMaybes $ exceptionTypeMatches expected <$> actuals
    in case matches of
        [] -> return ()
        (x:_) -> assertFailure x

can'tResolveAsBool :: TSException X.Range
can'tResolveAsBool = Filter $ Can'tResolveAsBool dummyRange "" TAny

duplicateName :: TSException X.Range
duplicateName = Filter $ DuplicateName dummyRange ""

funcArgumentWrongType :: TSException X.Range
funcArgumentWrongType = Filter $ FuncArgWrongType dummyRange "" 0 TAny TAny

canOnlyFilterDirectory :: TSException X.Range
canOnlyFilterDirectory = Other $ CanOnlyFilterDirectory ""

can'tParse :: TSException X.Range
can'tParse = Other Can'tParse

exceptionTypeMatches :: (TSException a) -> (TSException a) -> Maybe String
exceptionTypeMatches (Filter x) (Filter y) = filterExceptionTypeMatches x y
exceptionTypeMatches (Other x) (Other y) = otherExceptionTypeMatches x y
exceptionTypeMatches x y = Just $ "Errors did not match: " <> (show x) <> " and " <> (show y)

otherExceptionTypeMatches :: OtherException -> OtherException -> Maybe String
otherExceptionTypeMatches
    (CanOnlyFilterDirectory {})
    (CanOnlyFilterDirectory {}) = Nothing
otherExceptionTypeMatches
    (Can'tParse {})
    (Can'tParse {}) = Nothing
otherExceptionTypeMatches
    (FailedToReadTree {})
    (FailedToReadTree {}) = Nothing
otherExceptionTypeMatches x y = Just $ "Errors did not match: " <> (show x) <> " and " <> (show y)

filterExceptionTypeMatches :: FilterException a -> FilterException a -> Maybe String
filterExceptionTypeMatches
    (Can'tResolveAsBool {})
    (Can'tResolveAsBool {}) = Nothing
filterExceptionTypeMatches
    (DuplicateName {})
    (DuplicateName {}) = Nothing
filterExceptionTypeMatches
    (FuncArgWrongType {})
    (FuncArgWrongType {}) = Nothing
filterExceptionTypeMatches
    (FuncNameNotRecognized {})
    (FuncNameNotRecognized {}) = Nothing
filterExceptionTypeMatches
    (FuncWrongNumArgs {})
    (FuncWrongNumArgs {}) = Nothing
filterExceptionTypeMatches
    (ListNotHeterogeneous {})
    (ListNotHeterogeneous {}) = Nothing
filterExceptionTypeMatches
    (NotAFunction {})
    (NotAFunction {}) = Nothing
filterExceptionTypeMatches
    (UnrecognizedName {})
    (UnrecognizedName {}) = Nothing
filterExceptionTypeMatches x y = Just $ "Errors did not match: " <> (show x) <> " and " <> (show y)

{-|
  Applies two filters to the same tree, and a function which compares the resultant
  two filtered trees to the original tree. Mostly useful for completeness checks, ie
  ensuring that what has been filtered out by one string is the inverse of what has
  been filtered out by that string's inverse.
-}
applyTwoFiltersAndCompare ::
    FileName -> LBS.ByteString -> LBS.ByteString -> (DirTree FData -> DirTree FData -> DirTree FData -> IO()) -> IO ()
applyTwoFiltersAndCompare dirName filtA filtB compareF = do
    origTreeE <- getFDataTree dirName
    let filteredTreeAE = filterTreeWith filtA =<< origTreeE
    let filteredTreeBE = filterTreeWith filtB =<< origTreeE
    case (origTreeE, filteredTreeAE, filteredTreeBE) of
        (Left (e:_), _, _) -> throw e
        (_, Left (e:_), _) -> throw e
        (_, _, Left (e:_)) -> throw e
        (Right origTree, Right filteredA, Right filteredB) -> compareF origTree filteredA filteredB
        _ -> error "Unexpected outcome"

{-|
  Takes a single tree and single filter, and then compares the original tree (expressed
  as a bash array), the filtered tree, and the --exclude filtered tree. Mostly useful
  for completeness checks, ie to ensure that what has been excluded + what has been
  included equals the original.
-}
compareInclExcl :: FileName -> LBS.ByteString -> ([String] -> [String] -> [String] -> IO()) -> IO ()
compareInclExcl dirName filtStr compareF = do
    origTreeE <- getFDataTree dirName
    let filteredTreeE = filterTreeWith filtStr =<< origTreeE
    let origArrayE = toBashArray (Just AlwaysDirs) <$> origTreeE
    let inclArrayE = toBashArray (Just AlwaysDirs) <$> filteredTreeE
    let exclArrayE = getExcluded (Just AlwaysDirs) <$> origTreeE <*> filteredTreeE
    case (origArrayE, inclArrayE, exclArrayE) of
        (Left (e:_), _, _) -> throw e
        (_, Left (e:_), _) -> throw e
        (_, _, Left (e:_)) -> throw e
        (Right origArray, Right inclArray, Right exclArray) -> compareF origArray inclArray exclArray
        _ -> error "Received neither errors nor result"

main :: IO ()
main = do
    _ <- createDirectoryIfMissing False $ testDataPath </> "c-executable" </> "emptydir"
    hspec $ do
        -- The empty directory needed for testing may be missing, if the $src has been copied
        describe "Simple boolean expressions" $ do
            it "True" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "True"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "(True)" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "(True)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "! True" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "! True"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "(! True)" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "(! True)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "! (True)" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "! (True)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "True & True" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "True & True"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "True & (! True)" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "True & (! True)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "True & ! True" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "True & ! True"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "False" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "False"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "False | True" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "False | True"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "More complex logical expressions" $ do
            it "False | ! (! False)" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "False | ! (! False)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "(False | True) & ((|) False False)" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "(False | True) & ((|) False False)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Equality checks" $ do
            it "True == True" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "True == True"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "[ 1, 2, 3 ] == [1, 2, 3]" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "[ 1, 2, 3 ] == [1, 2, 3]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "[ 1, 2, 3 ] == [1, 2, 4]" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "[ 1, 2, 3 ] == [1, 2, 4]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Simple let expressions" $ do
            it "let isFalse = False; in isFalse" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "let isFalse = False; in isFalse"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let isFalse = False in isFalse" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "let isFalse = False in isFalse"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let isFalse = False; isTrue = True; in isFalse & isTrue" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "let isFalse = False; isTrue = True; in isFalse & isTrue"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let isFalse = False; isTrue = True in isFalse | isTrue" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "let isFalse = False; isTrue = True in isFalse | isTrue"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "String matching" $ do
            it "basename file == \"file_a_1.cpp\"" $ do
                let treeA' = filterDir (\dt -> (LBS.pack $ name dt) == "file_a_1.cpp") treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "basename file == \"file_a_1.cpp\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "basename file == 'file_a_1.cpp'" $ do
                let treeA' = filterDir (\dt -> (LBS.pack $ name dt) == "file_a_1.cpp") treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "basename file == 'file_a_1.cpp'"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Nested let expressions" $ do
            it "let x = {}; in let y = {}; in ..." $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
                let treeC' = filterDir (\dt -> LBS.isSuffixOf ".hs" $ LBS.pack $ name dt) treeC
                let expected = Dir "test-data" [ treeA', treeB', treeC' ]
                let testStr = "let x = \"cpp\"; in let y = \"hs\"; in endsWith x (basename file) | endsWith y (basename file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let x = {} in let y = {} in ..." $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
                let treeC' = filterDir (\dt -> LBS.isSuffixOf ".hs" $ LBS.pack $ name dt) treeC
                let expected = Dir "test-data" [ treeA', treeB', treeC' ]
                let testStr = "let x = \"cpp\" in let y = \"hs\" in endsWith x (basename file) | endsWith y (basename file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let x = {}; in let y = {} in ..." $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
                let treeC' = filterDir (\dt -> LBS.isSuffixOf ".hs" $ LBS.pack $ name dt) treeC
                let expected = Dir "test-data" [ treeA', treeB', treeC' ]
                let testStr = "let x = \"cpp\"; in let y = \"hs\" in endsWith x (basename file) | endsWith y (basename file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let x = {} in let y = {}; in ..." $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
                let treeC' = filterDir (\dt -> LBS.isSuffixOf ".hs" $ LBS.pack $ name dt) treeC
                let expected = Dir "test-data" [ treeA', treeB', treeC' ]
                let testStr = "let x = \"cpp\" in let y = \"hs\"; in endsWith x (basename file) | endsWith y (basename file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Conflicting let expressions" $ do
            it "Inner let and outer let co-exist" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "let x = True; in let y = False in x & y"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "Inner lets shadow outer lets" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "let x = True; in let x = False; in x"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "Conflicts within a single let declare clause" $ do
                let testStr = "let x = True; x = False; in x"
                returnsExceptionOfType testStr duplicateName treeTestData
        describe "General let expression tests" $ do
            it "let x = {Bool}; in x" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "let isCpp = (False); in (False | True) & ((|) False isCpp)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let x = {matcher}; in x" $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
                let expected = Dir "test-data" [ treeA' , treeB' ]
                let testStr = "let isCpp = (endsWith \".cpp\" (basename file)); in isCpp"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let x = {matcher}; in (x & exp)" $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "let isCpp = (endsWith \".cpp\" (basename file));"
                              <> " in ( isCpp & (startsWith \"file_a\" (basename file)))"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let x = {matcher1} in let y = {matcher2} in (x & y)" $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "let isCpp = endsWith \".cpp\" (basename file);"
                              <> " in let isFileA = startsWith \"file_a\" (basename file);"
                              <> " in (isCpp & isFileA)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "let x = {matcher1}; y = {matcher2}; in (x & y)" $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "let isCpp = endsWith \".cpp\" (basename file);"
                              <> " isFileA = startsWith \"file_a\" (basename file)"
                              <> " in (isCpp & isFileA)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "Let expression with three values set" $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let treeC' = filterDir (\dt -> LBS.isSuffixOf ".hs" $ LBS.pack $ name dt) treeC
                let expected = Dir "test-data" [ treeA', treeC' ]
                let testStr = "let isCpp = endsWith \".cpp\" (basename file);"
                              <> " isFileA = startsWith \"file_a\" (basename file);"
                              <> " isHs = endsWith \".hs\" (basename file);"
                              <> " in (isCpp & isFileA) | isHs"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "Two partially applied functions, one fully applied function in let" $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
                let treeC' = filterDir (\dt -> LBS.isSuffixOf ".hs" $ LBS.pack $ name dt) treeC
                let expected = Dir "test-data" [ treeA', treeB', treeC' ]
                let testStr = "let x = endsWith \"cpp\"; y = endsWith \"hs\"; " <>
                              "in let fname = basename file in x fname | y fname"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "partially applied function in let" $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeA
                let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" $ LBS.pack $ name dt) treeB
                let expected = Dir "test-data" [ treeA', treeB' ]
                let testStr = "let isCpp = endsWith \"cpp\"; in isCpp (basename file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Comparison operators" $ do
            it "4 <= 5" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "4 <= 5"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "4 <= 4" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "4 <= 4"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "4 <= 3" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "4 <= 3"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "4 >= 3" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "4 >= 3"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "4 >= 4" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "4 >= 4"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "4 >= 5" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "4 >= 5"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "basename file == <string>" $ do
            it "basename file == \"string\"" $ do
                let treeA' = filterDir (\dt -> (name dt) == "file_a_2.hpp" ) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "basename file == \"file_a_2.hpp\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "(basename file) == \"string\"" $ do
                let treeA' = filterDir (\dt -> (name dt) == "file_a_2.hpp" ) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "(basename file) == \"file_a_2.hpp\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "(basename file == \"string\")" $ do
                let treeA' = filterDir (\dt -> (name dt) == "file_a_2.hpp" ) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "(basename file == \"file_a_2.hpp\")"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "basename file == \"string\" for file in nested directory" $ do
                let treeA' = filterDir (\dt -> LBS.isPrefixOf "docs" (LBS.pack $ name dt)) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "(basename file) == \"docs.md\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "basename file == \"string\" for directory" $ do
                let treeA' = filterDir (\dt -> name dt == "docs") treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "(basename file) == \"docs\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "basename file == \"string\" for directory in nested directory" $ do
                let treeC' = Dir "c-executable" [ Dir "ext" [ ] ]
                let expected = Dir "test-data" [ treeC' ]
                let testStr = "(basename file) == \"ext\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "parents file" $ do
            it "parents file == [ \"b-library\" ] & isFile file" $ do
                let treeB' = filterDir (\dt -> LBS.isPrefixOf "file_b" (LBS.pack $ name dt)) treeB
                let expected = Dir "test-data" [ treeB' ]
                let testStr = "parents file == [ \"b-library\" ] & isFile file"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "elem \"docs\" (parents file)" $ do
                let treeA' = filterDir (\dt -> LBS.isPrefixOf "docs" (LBS.pack $ name dt)) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "elem \"docs\" (parents file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "any (startsWith \"a-\") (parents file)" $ do
                let expected = Dir "test-data" [ treeA ]
                let testStr = "any (startsWith \"a-\") (parents file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "all (occursIn \"ex\") (parents file) & (length (parents file)) > 0" $ do
                let expected = Dir "test-data" [ treeC ]
                let testStr = "all (occursIn \"ex\") (parents file) & (length (parents file)) > 0"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "startsWith <string> <string>" $ do
            it "startsWith \"myo\" \"myopic\"" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "startsWith \"myo\" \"myopic\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "startsWith \"searchStr\" (basename file)" $ do
                let treeA' = filterDir (\dt -> LBS.isPrefixOf "docs" (LBS.pack $ name dt)) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "startsWith \"docs\" (basename file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "startsWith \"file_a\" (basename file)" $ do
                let treeA' = Dir "a-project" [ File "file_a_1.cpp" () , File "file_a_2.hpp" () ]
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "startsWith \"file_a\" (basename file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "endsWith <string> <string>" $ do
            it "endsWith \".cpp\" (basename file)" $ do
                let treeA' = filterDir (\dt -> LBS.isSuffixOf ".cpp" (LBS.pack $ name dt)) treeA
                let treeB' = filterDir (\dt -> LBS.isSuffixOf ".cpp" (LBS.pack $ name dt)) treeB
                let expected = Dir "test-data" [ treeA', treeB' ]
                let testStr = "endsWith \".cpp\" (basename file)"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Logical combinations" $ do
            it "basename file == \"file_a_1.cpp\" | basename file == \"file_b_2.hpp\"" $ do
                let treeA' = filterDir (\dt -> (name dt) == "file_a_1.cpp" ) treeA
                let treeB' = filterDir (\dt -> (name dt) == "file_b_2.hpp" ) treeB
                let expected = Dir "test-data" [ treeA', treeB' ]
                let testStr = "basename file == \"file_a_1.cpp\" | basename file == \"file_b_2.hpp\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "(basename file == \"file_a_1.cpp\" | basename file == \"file_a_1.cpp\")" $ do
                let treeA' = filterDir (\dt -> (name dt) == "file_a_1.cpp" ) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "basename file == \"file_a_1.cpp\" | basename file == \"file_a_1.cpp\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "basename file == \"file_a_1.cpp\" & (basename file == \"file_b_2.hpp\")" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "basename file == \"file_a_1.cpp\" & basename file == \"file_b_2.hpp\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "isDir file" $ do
            it "isDir file" $ do
                let treeA' = Dir "a-project" [ Dir "docs" [] ]
                let treeB' = Dir "b-library" [ Dir ".cache" [] ]
                let treeC' = Dir "c-executable" [ Dir "ext" [] , Dir "emptydir" [] ]
                let expected = Dir "test-data" [ treeA', treeB', treeC' ]
                let testStr = "isDir file"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "elem <elem> <list>" $ do
            it "elem 1 [ 1, 2, 3 ]" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "elem 1 [ 1, 2, 3 ]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "elem (basename file) [ \"file_a_1.cpp\", \"file_b_2.hpp\" ]" $ do
                let treeA' = filterDir (\dt -> (name dt) == "file_a_1.cpp" ) treeA
                let treeB' = filterDir (\dt -> (name dt) == "file_b_2.hpp" ) treeB
                let expected = Dir "test-data" [ treeA', treeB' ]
                let testStr = "elem (basename file) [ \"file_a_1.cpp\", \"file_b_2.hpp\" ]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "elem 0 [1, 2, 3]" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "elem 0 [1, 2, 3]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "elem (basename file) [\"not_a_file_at_all.cpp\"]" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "elem (basename file) [\"not_a_file_at_all.cpp\"]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Partially applied functions" $ do
            it "((+) 1) 1 == 2" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "((+) 1) 1 == 2"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "all ( endsWith \"cpp\" ) [ \"this.cpp\" ]" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "all ( endsWith \"cpp\" ) [ \"this.cpp\" ]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "all ( endsWith \"cpp\" ) [ \"this.cpp\", \"this.hs\" ]" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "all ( endsWith \"cpp\" ) [ \"this.cpp\", \"this.hs\" ]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "(basename) file == \"string\"" $ do
                let treeA' = filterDir (\dt -> (name dt) == "file_a_2.hpp" ) treeA
                let expected = Dir "test-data" [ treeA' ]
                let testStr = "(basename) file == \"file_a_2.hpp\""
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Length functions" $ do
            it "length [1, 2] == 2" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "length [1, 2] == 2"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "length [] == 0" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "length [] == 0"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "length [] == 1" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "length [] == 1"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "length \"easygoing?!\" == 11" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "length \"easygoing?!\" == 11"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "length \"easygoing?!\" == 10" $ do
                let expected = Dir "test-data" [ ]
                let testStr = "length \"easygoing?!\" == 10"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Map functions" $ do
            it "map ((+) 1) [1, 2] == [2, 3]" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "map ((+) 1) [1, 2] == [2, 3]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "map ! [True, True] == [False, False]" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "map ! [True, True] == [False, False]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
            it "map ((!)) ([True, True]) == [False, False]" $ do
                let expected = Dir "test-data" [ treeA, treeB, treeC ]
                let testStr = "map ((!)) ([True, True]) == [False, False]"
                applyFilterWith testDataPath ( compareToExpected expected ) testStr
        describe "Correct exception types" $ do
            it "( True" $ do
                let testStr = "( True"
                returnsExceptionOfType testStr can'tParse treeTestData
            it "String does not resolve to Bool" $ do
                let testStr = "\"a string\""
                returnsExceptionOfType testStr can'tResolveAsBool treeTestData
            it "File cannot be filtered" $ do
                let testStr = "True"
                let filePath = "test/test-data/a-project/file_a_1.cpp"
                treeE <- getFDataTree filePath
                case treeE of
                    Right _ -> assertFailure "Incorrectly filtered a file"
                    Left errs -> returnsExceptionOfType' canOnlyFilterDirectory errs
            it "duplicate names cannot exist in expression" $ do
                let testStr = "let foo = True; foo = False; in foo"
                returnsExceptionOfType testStr duplicateName treeTestData
            it "startsWith function cannot have Bool as first argument" $ do
                let testStr = "startsWith True \"a string\""
                returnsExceptionOfType testStr funcArgumentWrongType treeTestData
        describe "Documentation examples" $ do
            it "elem \"mySrc\" (parents file) & !(isDir file)" $ do
                let expected = getTestsDir [ mySrcDir ]
                let testStr = "elem \"mySrc\" (parents file) & !(isDir file)"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "basename file == \"myMain.rs\"" $ do
                let expected = getTestsDir [ mySrcDir ]
                let testStr = "basename file == \"myMain.rs\""
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "basename file == \"docs.md\" | basename file == \"tests.rs\"" $ do
                let expected = getTestsDir [ docsDir, testsDir ]
                let testStr = "basename file == \"docs.md\" | basename file == \"tests.rs\""
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "1 == 1" $ do
                let expected = getTestsDir [ docsDir, mySrcDir, testsDir ]
                let testStr = "1 == 1"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "False" $ do
                let expected = Dir "docs-test-data" [ ]
                let testStr = "False"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "!(occursIn \"tests\" (basename file))" $ do
                let expected = getTestsDir [ docsDir, mySrcDir ]
                let testStr = "!(occursIn \"tests\" (basename file))"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "let isDocs = basename file == \"docs.md\"; isTests = basename file == \"tests.rs\" in isDocs | isTests" $ do
                let expected = getTestsDir [ docsDir, testsDir ]
                let testStr = "let isDocs = basename file == \"docs.md\"; isTests = basename file == \"tests.rs\" in isDocs | isTests"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "endsWith \".rs\" (basename file)" $ do
                let expected = getTestsDir [ mySrcDir, testsDir ]
                let testStr = "endsWith \".rs\" (basename file)"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "startsWith \"docs\" (basename file) | occursIn \"Main\" (basename file)" $ do
                let expected = getTestsDir [ docsDir, mySrcDir ]
                let testStr = "startsWith \"docs\" (basename file) | occursIn \"Main\" (basename file)"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "let isRust = endsWith \".rs\" in isRust (basename file)" $ do
                let expected = getTestsDir [ mySrcDir, testsDir ]
                let testStr = "let isRust = endsWith \".rs\" in isRust (basename file)"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "parents file == [ \"myDir\", \"docs\" ]" $ do
                let expected = getTestsDir [ docsDir ]
                let testStr = "parents file == [ \"myDir\", \"docs\" ]"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "all (startsWith \"my\") (parents file) & (length (parents file)) > 0" $ do
                let expected = getTestsDir [ mySrcDir ]
                let testStr = "all (startsWith \"my\") (parents file) & (length (parents file)) > 1"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "any ((==) \"tests\") (parents file)" $ do
                let expected = getTestsDir [ testsDir ]
                let testStr = "any ((==) \"tests\") (parents file)"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "any ((==) 4) (map length (parents file))" $ do
                let expected = getTestsDir [ docsDir ]
                let testStr = "any ((==) 4) (map length (parents file))"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
            it "any ((==) \"tests\") (parents file)" $ do
                let expected = getTestsDir [ testsDir ]
                let testStr = "any ((==) \"tests\") (parents file)"
                applyFilterWith docsTestDataPath ( compareToExpected expected ) testStr
        describe "Bash array functions" $ do
            it "Correctly includes" $ do
                let testStr = "endsWith \"cpp\" (basename file)"
                let expected = [
                        "a-project/file_a_1.cpp"
                        , "b-library/file_b_1.cpp"
                        , "a-project"
                        , "b-library" ]
                let compFunc = \f -> (sort $ toBashArray (Just AlwaysDirs) f) `shouldBe` (sort expected)
                applyFilterWith testDataPath compFunc testStr
            it "Correctly excludes" $ do
                let testStr = "let fn = basename file in any ((==) fn) [\"docs.md\", \"tree\"] | endsWith \".hs\" fn"
                let expected = [
                        "a-project/file_a_1.cpp"
                        , "a-project/file_a_2.hpp"
                        , "b-library/file_b_1.cpp"
                        , "b-library/file_b_2.hpp"
                        , "b-library/file_b_3.tmp"
                        , "b-library/.cache"
                        , "b-library/.cache/temp_b_3.tmp"
                        , "b-library"
                        , "c-executable/emptydir"
                        ]
                let compFunc = \o f -> (sort $ getExcluded (Just AlwaysDirs) f o)
                                        `shouldBe`
                                        (sort expected)
                applyFilterWithComparative testDataPath compFunc testStr
            it "Includes plus Excludeds equals total" $ do
                let testStr = "let fn = basename file in any ((==) fn) [\"docs.md\", \"tree\"] | endsWith \".hs\" fn"
                let compareF = \orig incl excl -> (sort $ incl ++ excl) `shouldBe` (sort orig)
                compareInclExcl testDataPath testStr compareF

