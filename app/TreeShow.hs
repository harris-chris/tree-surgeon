module TreeShow
  (
    showTree
    , showTreeComparison
  ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import System.Console.ANSI
import System.Directory.Tree

singleInd :: String
singleInd = "   "

crossJoiner :: Char
crossJoiner = '├'

substituteJoiner :: Char -> String -> String
substituteJoiner joiner str =
    let indWidth = length singleInd
    in if length str > 0
        then take (length str - indWidth) str <> (joiner:"──")
        else ""

lastJoiner :: Char
lastJoiner = '└'

showTree :: DirTree a -> String
showTree tree = showTree' "" True tree

showTree' :: String -> Bool -> DirTree a -> String
showTree' prelimStr isLast (Dir name contents) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> setDirFormat name
        prelimStr' = prelimStr <> "│  "
        subLines = showTree' prelimStr' False <$> (init contents)
        lastPrelimStr = prelimStr <> singleInd
        lastLine = showTree' lastPrelimStr True (last contents)
    in init $ unlines $ thisLineStr:subLines ++ [lastLine]
showTree' prelimStr isLast (File name _) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> name
    in thisLineStr

data Status = Present | Removed

setStatusPrefix :: Status -> String -> String
setStatusPrefix Present prelimStr = ' ':(tail prelimStr)
setStatusPrefix Removed prelimStr = setRed "-" <> (tail prelimStr)

setDirFormat :: String -> String
setDirFormat dirName =
    setSGRCode [SetConsoleIntensity BoldIntensity]
    <> setSGRCode [SetColor Foreground Vivid Blue]
    <> dirName
    <> setSGRCode [Reset]

setRed :: String -> String
setRed str = setSGRCode [SetColor Foreground Vivid Red] <> str <> setSGRCode [Reset]

type Zipped a = [(Bool, Maybe (DirTree a), DirTree a)]

zipContents :: Eq a => [DirTree a] -> [DirTree a] -> Zipped a -> Zipped a
zipContents (x':[])  (x:[]) zipped = zipContents [] [] $ (True, Just x', x):zipped
zipContents [] (x:[]) zipped = zipContents [] [] $ (True, Nothing, x):zipped
zipContents [] (x:xs) zipped = zipContents [] xs $ (False, Nothing, x):zipped
zipContents (x':xs') (x:xs) zipped =
    if x' == x
        then zipContents xs' xs $ (False, Just x', x):zipped
        else zipContents (x':xs') xs $ (False, Nothing, x):zipped
zipContents _ [] zipped = reverse zipped

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

showTreeComparison :: Ord a => DirTree a -> DirTree a -> String
showTreeComparison original filtered =
    showTreeComparison' Nothing True (Just filtered) original

showTreeComparison' :: Ord a => Maybe String -> Bool -> Maybe (DirTree a) -> DirTree a -> String
showTreeComparison' Nothing isLast treeM tree =
    let tree' = sortDir tree
        treeM' = sortDir <$> treeM
        prelimStr = Just " "
    in showTreeComparison' prelimStr isLast treeM' tree'
showTreeComparison' (Just prelimStr) isLast (Just (File name' _)) (File name _) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> name
    in setStatusPrefix Present thisLineStr
showTreeComparison' (Just prelimStr) isLast Nothing (File name _) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> setRed name
    in setStatusPrefix Removed thisLineStr
showTreeComparison' (Just prelimStr) isLast (Just (Dir name' contents')) (Dir name contents) =
    let joiner = if isLast then '└' else '├'
        prelimStr' = setStatusPrefix Present prelimStr
        thisLineStr = substituteJoiner joiner prelimStr' <> setDirFormat name
        zippedContents = zipContents contents contents' []
        prelimStr'' = prelimStr' <> "│  "
        subLines = (uncurry3 $ (showTreeComparison' (Just prelimStr''))) <$> (init zippedContents)
        lastPrelimStr = prelimStr' <> singleInd
        lastLine = uncurry3 (showTreeComparison' (Just lastPrelimStr)) (last zippedContents)
    in init $ unlines $ thisLineStr:subLines ++ [lastLine]
showTreeComparison' (Just prelimStr) isLast Nothing (Dir name contents) =
    let joiner = if isLast then '└' else '├'
        prelimStr' = setStatusPrefix Present prelimStr
        thisLineStr = substituteJoiner joiner prelimStr' <> setDirFormat name
        prelimStr'' = prelimStr' <> "│  "
        subLines = showTreeComparison' (Just prelimStr'') False Nothing <$> (init contents)
        lastPrelimStr = prelimStr' <> singleInd
        lastLine = showTreeComparison' (Just lastPrelimStr) True Nothing (last contents)
    in init $ unlines $ thisLineStr:subLines ++ [lastLine]

