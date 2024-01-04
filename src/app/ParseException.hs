module ParseException
    (
        parseError
    ) where

import qualified Lexer as L

parseError :: (L.RangedToken, [String]) -> L.Alex a
parseError (L.RangedToken L.EOF _, _) =
    L.alexError "Filter string ended unexpectedly - did you forget to close a bracket?"
parseError _ = do
    (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
    L.alexError $ "Parse error in text " <>
        " at line " <> show line <> ", column " <> show column

