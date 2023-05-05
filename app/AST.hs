module AST
  (
    Exp(..)
    , StringAtom(..)
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
    IsChildOf a (StringAtom a)
    | NameEndsWith a (StringAtom a)
    | Or a (Exp a) (Exp a)
    | And a (Exp a) (Exp a)
    | EPar a (Exp a)
    deriving (Foldable, Show)

data StringAtom a =
    StringString a ByteString
    | StringInteger a Integer
    | StringPar a (StringAtom a)
    | StringList a [StringAtom a]
    deriving (Foldable, Show)

instance Show a => FiltersFsObjData (Exp a) where
    filterObjData (IsChildOf _ (StringString _ x)) name objData = elem x $ parents objData
    filterObjData (IsChildOf _ (StringList _ xs)) name objData =
        any (\(StringString _ x) -> elem x $ parents objData) xs
    filterObjData (NameEndsWith _ (StringString _ x)) name objData = isSuffixOf x name
    filterObjData (Or _ x y) name objData =
        (filterObjData x name objData) || (filterObjData y name objData)
    filterObjData (And _ x y) name objData =
        (filterObjData x name objData) && (filterObjData y name objData)
    filterObjData (EPar _ x) name objData = filterObjData x name objData


