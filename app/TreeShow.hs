module TreeShow where

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

showTree :: String -> Bool -> DirTree a -> String
showTree prelimStr isLast (Dir name contents) =
    let joiner = if isLast then '└' else '├'
        thisLineStr =
            substituteJoiner joiner prelimStr
            <> setSGRCode [SetConsoleIntensity BoldIntensity]
            <> setSGRCode [SetColor Foreground Vivid Blue]
            <> name
            <> setSGRCode [Reset]
        prelimStr' = prelimStr <> "│  "
        subLines = showTree prelimStr' False <$> (init contents)
        lastPrelimStr' = prelimStr <> singleInd
        lastLine = showTree lastPrelimStr' True (last contents)
    in init $ unlines $ thisLineStr:subLines ++ [lastLine]
showTree prelimStr isLast (File name _) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> name
    in thisLineStr

