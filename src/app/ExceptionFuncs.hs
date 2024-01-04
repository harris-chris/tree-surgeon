{-# LANGUAGE OverloadedStrings #-}

module ExceptionFuncs
  (
    toErrorMessage
  ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text as T
import TextShow

import Exceptions
import IsTrace

errorMessageFromFilterStr :: IsTrace trc =>
    BS.ByteString -> FilterException trc -> Builder
errorMessageFromFilterStr origTxt raw@(Can'tResolveAsBool rep _ _) =
    unlinesB [
        showb raw
        , fromText "in original source:"
        , trace (T.pack $ BS.unpack origTxt) rep
        ]
errorMessageFromFilterStr origTxt raw@(DuplicateName rep _) =
    unlinesB [
        (showb raw)
        , fromText "in original source:"
        , trace (T.pack $ BS.unpack origTxt) rep
        ]
errorMessageFromFilterStr origTxt raw@(FuncArgWrongType rep _ _ _ _) =
    unlinesB [
        showb raw
        , fromText "in original source:"
        , trace (T.pack $ BS.unpack origTxt) rep
        ]
errorMessageFromFilterStr origTxt raw@(FuncNameNotRecognized rep _) =
    unlinesB [
        showb raw
        , fromText "in original source:"
        , trace (T.pack $ BS.unpack origTxt) rep
        ]
errorMessageFromFilterStr origTxt raw@(FuncWrongNumArgs rep _ _ _) =
    unlinesB [
        showb raw
        , fromText "in original source:"
        , trace (T.pack $ BS.unpack origTxt) rep
        ]
errorMessageFromFilterStr _ raw = showb raw

toErrorMessage :: IsTrace trc =>
    BS.ByteString -> TSException trc -> String
toErrorMessage filterStr (Filter filterExc) = T.unpack $ T.strip $ toText $ errorMessageFromFilterStr filterStr filterExc
toErrorMessage _ (Other otherExc) = show otherExc

