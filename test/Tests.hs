import Prelude hiding (Word, getLine)

import Test.Hspec

import Debug.Trace (trace, traceShowId)
import System.FilePath
import System.Directory

testData :: FilePath
testData = "test/test-data"

testDataComp :: FilePath
testDataComp = testData </> "comp"

testDataTemp :: FilePath
testDataTemp = testData </> "temp"

main :: IO ()
main = hspec $ do
    describe "testThing" $ do
        it "fucks with my head" $ do
            1 `shouldBe` 1
