{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_tree_surgeon (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "tree_surgeon"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Define and run filters for directory trees"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
