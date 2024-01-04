module Output
  (
    applyPaths
    , showTree
    , showTreeComparison
    , toBashArray
  ) where

import Data.List
import Debug.Trace
import System.Console.ANSI
import System.Directory.Tree
import System.FilePath

import Cli

singleInd :: String
singleInd = "    "

applyPaths :: FileName -> DirTree a -> DirTree a
applyPaths source (Dir name conts) =
    let name' = normalise $ source </> name
        conts' = applyPaths source <$> conts
    in Dir name' conts'
applyPaths source (File name x) = File ( normalise $ source </> name ) x

substituteJoiner :: Char -> String -> String
substituteJoiner joiner str =
    let indWidth = length singleInd
    in if length str > 1
        then take (length str - indWidth) str <> (joiner:"── ")
        else str

showTree :: DirTree a -> String
showTree tree = showTree' "" True tree

showTree' :: String -> Bool -> DirTree a -> String
showTree' prelimStr isLast (Dir name contents) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> setDirFormat name
        allLines =
                  if length contents == 0
                  then [thisLineStr]
                  else
                      let prelimStr' = prelimStr <> "│   "
                          subLines = showTree' prelimStr' False <$> (init contents)
                          lastPrelimStr = prelimStr <> singleInd
                          lastLine = showTree' lastPrelimStr True (last contents)
                      in thisLineStr:subLines ++ [lastLine]
    in stripNewLines $ unlines allLines
showTree' prelimStr isLast (File name _) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> name
    in thisLineStr
showTree' _ _ (Failed _ _) = error "Failed encountered in showTree'"

data Status = Present | Removed

setStatusPrefix :: Status -> String -> String
setStatusPrefix Present prelimStr =
    if length prelimStr > 0 then ' ':(tail prelimStr) else " "
setStatusPrefix Removed prelimStr =
    if length prelimStr > 0 then setRed "-" <> (tail prelimStr) else setRed "-"

setDirFormat :: String -> String
setDirFormat dirName =
    setSGRCode [SetConsoleIntensity BoldIntensity]
    <> setSGRCode [SetColor Foreground Vivid Blue]
    <> dirName
    <> setSGRCode [Reset]

setRed :: String -> String
setRed str =
    setSGRCode [SetColor Foreground Vivid Red]
    <> str
    <> setSGRCode [Reset]

type Zipped a = [(Bool, Maybe (DirTree a), DirTree a)]

-- Note that this is sensitive to order - the original comes first, then the filtered
isFilteredOf :: Eq a => DirTree a -> DirTree a -> Bool
isFilteredOf f@(File _ _) f'@(File _ _) = f == f'
isFilteredOf (Dir name contents) (Dir name' contents') = name == name'
isFilteredOf _ _ = False

-- The contents are going to be different, if a directory; so we just want to check
-- parents and name, not contents; hence the use of isFilteredOf rather than ==
zipContents :: Eq a => [DirTree a] -> [DirTree a] -> Zipped a -> Zipped a
zipContents (x':[])  (x:[]) zipped = zipContents [] [] $ (True, Just x', x):zipped
zipContents [] (x:[]) zipped = zipContents [] [] $ (True, Nothing, x):zipped
zipContents [] (x:xs) zipped = zipContents [] xs $ (False, Nothing, x):zipped
zipContents (x':xs') (x:xs) zipped =
    if isFilteredOf x x'
        then zipContents xs' xs $ (False, Just x', x):zipped
        else zipContents (x':xs') xs $ (False, Nothing, x):zipped
zipContents _ [] zipped = reverse zipped

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

stripNewLines :: String -> String
stripNewLines ('\n':xs) = stripNewLines xs
stripNewLines xs@(_:_) =
    if last xs == '\n'
    then init xs
    else xs
stripNewLines x = x

showTreeComparison :: (Ord a, Show a) => DirTree a -> DirTree a -> String
showTreeComparison original filtered =
    showTreeComparison' Nothing True (Just filtered) original

showTreeComparison' :: (Ord a, Show a) =>
    Maybe String -> Bool -> Maybe (DirTree a) -> DirTree a -> String
showTreeComparison' Nothing isLast treeM tree =
    let tree' = sortDir tree
        treeM' = sortDir <$> treeM
        prelimStr = Just ""
    in showTreeComparison' prelimStr isLast treeM' tree'
showTreeComparison' (Just prelimStr) isLast (Just (File _ _)) (File name _) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> name
    in setStatusPrefix Present thisLineStr
showTreeComparison' (Just prelimStr) isLast Nothing (File name _) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> setRed name
    in setStatusPrefix Removed thisLineStr
showTreeComparison' (Just prelimStr) isLast (Just (Dir _ [])) (Dir name []) =
    let joiner = if isLast then '└' else '├'
        prelimStr' = setStatusPrefix Present prelimStr
        thisLineStr = substituteJoiner joiner prelimStr' <> setDirFormat name
    in thisLineStr
showTreeComparison' (Just prelimStr) isLast (Just (Dir _ contents')) (Dir name contents) =
    let joiner = if isLast then '└' else '├'
        prelimStr' = setStatusPrefix Present prelimStr
        thisLineStr = substituteJoiner joiner prelimStr' <> setDirFormat name
        zippedContents = zipContents contents' contents []
        prelimStr'' = prelimStr' <> "│   "
        subLines = (uncurry3 $ (showTreeComparison' (Just prelimStr''))) <$> (init zippedContents)
        lastPrelimStr = prelimStr' <> singleInd
        lastLine = uncurry3 (showTreeComparison' (Just lastPrelimStr)) (last zippedContents)
    in stripNewLines $ unlines $ thisLineStr:subLines ++ [lastLine]
showTreeComparison' (Just prelimStr) isLast Nothing (Dir name []) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = setStatusPrefix Removed $
            substituteJoiner joiner prelimStr <> setRed name
    in thisLineStr
showTreeComparison' (Just prelimStr) isLast Nothing (Dir name contents) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = setStatusPrefix Removed $
            substituteJoiner joiner prelimStr <> setRed name
        prelimStr' = prelimStr <> "│   "
        subLines = showTreeComparison' (Just prelimStr') False Nothing <$> (init contents)
        lastPrelimStr = prelimStr <> singleInd
        lastLine = showTreeComparison' (Just lastPrelimStr) True Nothing (last contents)
    in stripNewLines $ unlines $ thisLineStr:subLines ++ [lastLine]

toBashArray :: Maybe TypeConstraint -> DirTree a -> [String]
toBashArray typeConstraint (Dir name conts) =
    let conts' = toBashArray' "." typeConstraint <$> conts
    in concat conts'
toBashArray _ (File _ _) = error "Tried to convert a single file into a bash array"

-- When traversing this tree, how do we know which directories have been included?
toBashArray' :: FileName -> Maybe TypeConstraint -> DirTree a -> [FileName]
-- If a dir has no contents, but is still part of the tree, it must have been explicitly included
toBashArray' path (Just FilesOnly) (Dir name []) = []
toBashArray' path _ (Dir name []) = [ normalise $ path </> name ]
toBashArray' path typeConstraint (Dir name conts) =
    let path' = normalise $ path </> name
        contsArrays = toBashArray' path' typeConstraint <$> conts
        contsFlat = concat contsArrays
        contsFlat' = normalise <$> contsFlat
    in case typeConstraint of
        (Just AlwaysDirs) -> path':contsFlat'
        _ -> contsFlat'
toBashArray' path (Just DirsOnly) (File name _) = []
toBashArray' path _ (File name _) = [normalise $ path </> name]

