module InfixOperators
    (
        InfixAssociativity(..)
        , InfixOperator
        , infixOperators
    ) where

import qualified Data.Text as T

data InfixAssociativity = RightAssoc | LeftAssoc

type InfixOperator = (T.Text, InfixAssociativity)

-- From low (tight binding) to high (loose binding)
infixOperators :: [InfixOperator]
infixOperators = [
        ("&", RightAssoc)
        , ("|", RightAssoc)
        , ("==", RightAssoc)
        , ("<", RightAssoc)
        , (">", RightAssoc)
        , ("<=", RightAssoc)
        , (">=", RightAssoc)
        , ("-", RightAssoc)
        , ("+", RightAssoc)
        , ("*", RightAssoc)
    ]
