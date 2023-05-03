import Prelude hiding (Word, getLine)

import Test.Hspec

import Debug.Trace (trace, traceShowId)
import System.FilePath

import TreeFilter

testData :: FilePath
testData = "test/test-data"

main :: IO ()
main = hspec $ do
    describe "filterTree" $ do
        it "Correctly reproduces a tree if include is used" $ do
            let origTree = getTree testData
            let filteredTree = filterDir "include" origTree
            origTree `shouldBe` filteredTree

