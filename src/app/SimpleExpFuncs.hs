module SimpleExpFuncs
    (
        rawExpToSimpleExp
    ) where

import ASTShow
import Exceptions
import IsTrace
import RawExp
import RawExpFuncs
import SimpleExp

rawExpToSimpleExp :: IsTrace trc => RawExp trc -> EitherF trc (SimpleExp trc)
rawExpToSimpleExp expr = mapRawExpToSimpleExp <$> simplifyRawExp expr

mapRawExpToSimpleExp :: IsTrace trc => RawExp trc -> SimpleExp trc
mapRawExpToSimpleExp (RawApply a xs) =
    SimpleApply a $ mapRawExpToSimpleExp <$> xs
mapRawExpToSimpleExp (RawIdent a name) = SimpleIdent a name
mapRawExpToSimpleExp (RawList a xs) = SimpleList a $ mapRawExpToSimpleExp <$> xs
mapRawExpToSimpleExp (RawLit a x) = SimpleLit a x
mapRawExpToSimpleExp expr = error $ show $ "Cannot map " <> (astShow 0 expr) <> " to SimpleExp"

