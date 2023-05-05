module AST
  (
    Exp(..)
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
    IsChildOf a (Exp a)
    | NameEndsWith a (Exp a)
    | Or a (Exp a) (Exp a)
    | And a (Exp a) (Exp a)
    | EPar a (Exp a)
    | EString a ByteString
    | EList a [Exp a]
    deriving (Foldable, Show)

instance Show a => FiltersFsObjData (Exp a) where
    filterObjData (IsChildOf _ (EString _ x)) name objData = elem x $ parents objData
    filterObjData (IsChildOf _ (EList z xs)) name objData =
        any (\(EString _ x) -> elem x $ parents objData) xs
    filterObjData (NameEndsWith _ (EString _ x)) name objData = isSuffixOf x name
    filterObjData (Or _ x y) name objData =
        (filterObjData x name objData) || (filterObjData y name objData)
    filterObjData (And _ x y) name objData =
        (filterObjData x name objData) && (filterObjData y name objData)
    filterObjData (EPar _ x) name objData = filterObjData x name objData
    filterObjData (EString _ x) name objData = True


