module Functions
  (
    nameF
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath

nameF :: ByteString -> ByteString
nameF pathStr = BS.pack $ last $ splitPath $ BS.unpack pathStr

