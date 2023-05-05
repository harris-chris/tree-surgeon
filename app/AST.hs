module AST
  (
    Exp(..)
    , StringParam(..)
    , FsObjData(..)
    , FiltersFsObjData(..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString, pack, isSuffixOf)
import Debug.Trace

data FsObjData =
    FileData { parents :: [ByteString] }
    deriving (Eq, Show)

class Show a => FiltersFsObjData a where
    filterObjData :: Show a => a -> ByteString -> FsObjData -> Bool

data Exp a =
    IsChildOf a (StringParam a)
    | NameEndsWith a (StringParam a)
    | Or a (Exp a) (Exp a)
    | And a (Exp a) (Exp a)
    | EPar a (Exp a)
    deriving (Foldable, Show)

data StringParam a =
    SString a ByteString
    | SPar a (StringParam a)
    | SList a [StringParam a]
    deriving (Foldable, Show)

instance Show a => FiltersFsObjData (Exp a) where
    filterObjData (IsChildOf _ (SString _ x)) name objData = elem x $ parents objData
    filterObjData (IsChildOf _ (SList _ xs)) name objData =
        any (\(SString _ x) -> elem x $ parents objData) xs
    filterObjData (NameEndsWith _ (SString _ x)) name objData = isSuffixOf x name
    filterObjData (Or _ x y) name objData =
        (filterObjData x name objData) || (filterObjData y name objData)
    filterObjData (And _ x y) name objData =
        (filterObjData x name objData) && (filterObjData y name objData)
    filterObjData (EPar _ x) name objData = filterObjData x name objData


