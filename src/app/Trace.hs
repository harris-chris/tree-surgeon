module Trace (
    Trace(..)
    , mkTrace
) where

import qualified Data.Text as T
import TextShow

import ASTShow (intercalate)
import IsTrace

data Trace a = Trace {
    focus :: a
    , clauses :: [Clause a]
}

instance (Semigroup a) => Semigroup (Trace a) where
    x <> y = Trace ((focus x) <> (focus y)) ((clauses x) ++ (clauses y))

data Clause a =
    OriginallyDefined a (Trace a)

mkTrace :: a -> Trace a
mkTrace a = Trace a []

getClauseTrace :: IsTrace a => T.Text -> Clause a -> Builder
getClauseTrace origTxt (OriginallyDefined refTrace cTrace) =
    "where " <> (trace origTxt refTrace)
    <> " is defined here " <> (trace origTxt cTrace)

instance IsTrace a => IsTrace (Trace a) where
    trace origTxt (Trace focus clauses) =
        let
            clauseBuilders = getClauseTrace origTxt <$> clauses
        in (trace origTxt focus) <> "\n" <> (intercalate "\n" clauseBuilders)

