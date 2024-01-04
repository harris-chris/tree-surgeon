{-# LANGUAGE OverloadedStrings #-}

module Lit
    (
        Lit(..)
    ) where

import qualified Data.Text as T
import TextShow

import ASTShow

data Lit =
    LBool Bool
    | LString T.Text
    | LInteger Int
    deriving Eq

instance ASTShow Lit where
    astShow i (LBool x) = (indent i) <> (fromString $ show x)
    astShow i (LString x) = (indent i) <> (fromString $ show x)
    astShow i (LInteger x) = (indent i) <> (fromString $ show x)
    -- astShow (LBool x) = "LBool " <> (T.pack $ show x)
    -- astShow (LString x) = "LString " <> (T.pack $ show x)
    -- astShow (LInteger x) = "LInteger " <> (T.pack $ show x)

