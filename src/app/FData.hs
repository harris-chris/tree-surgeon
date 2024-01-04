module FData
  (
    FData(..)
    , FileType(..)
    , getFilepath
  ) where

import Debug.Trace

import System.FilePath

import qualified Data.Text as T

data FileType = FileFileType | DirFileType
    deriving (Eq, Show, Ord)

data FData =
    FData {
        basename :: T.Text
        , parents :: [T.Text]
        , fileType :: FileType
        }
    deriving (Eq, Ord, Show)

getFilepath :: FData -> String
getFilepath fData =
    let parentsPath = T.unpack <$> parents fData
    in joinPath $ parentsPath ++ [T.unpack $ basename fData]


