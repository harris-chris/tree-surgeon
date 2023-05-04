module AST
  (
    Exp(..)
    , FsObjData(..)
    , FiltersFsObjData(..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

data FsObjData =
    FileData { parents :: [String] }
    deriving (Eq, Show)

class FiltersFsObjData a where
    filterObjData :: a -> name -> FsObjData -> Bool

data Exp a =
    IsChildOf a (Exp a)
    | Or a (Exp a) (Exp a)
    | EPar a (Exp a)
    | EString a ByteString
    deriving (Foldable, Show)

instance FiltersFsObjData (Exp a) where
    filterObjData (IsChildOf _ x) name objData = True
    filterObjData (Or _ x y) name objData = True
    filterObjData (EPar _ x) name objData = True
    filterObjData (EString _ x) name objData = True


