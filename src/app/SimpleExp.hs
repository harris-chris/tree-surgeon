{-# LANGUAGE OverloadedStrings #-}

module SimpleExp
  (
    SimpleExp(..)
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Lit

-- This is the simplest possible representation of RawExp.
-- All user-defined names have been removed.
-- Once names have been removed, we assume that all Idents are function calls.
-- Since all Idents are function calls, we can wrap them into the SimpleApply type.
data SimpleExp a =
    SimpleApply a (NE.NonEmpty (SimpleExp a))
    -- List
    | SimpleList a [SimpleExp a]
    -- Literals
    | SimpleLit a Lit
    | SimpleIdent a T.Text

