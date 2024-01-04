module ASTShow (
        ASTShow(..)
        , indent
        , intercalate
    ) where

import qualified Data.List as L
import qualified Data.Text as T
import TextShow

indentTxt :: T.Text
indentTxt = "    "

indent :: Int -> Builder
indent i = fromText $ T.replicate i indentTxt

intercalate :: T.Text -> [Builder] -> Builder
intercalate sep xs =
    L.foldl' (\acc x -> acc <> (fromText sep) <> x) mempty xs

class ASTShow a where
    -- errorShow :: a -> Builder
    astShow :: Int -> a -> Builder

