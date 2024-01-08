module ToGitExclude
    ( toGitExclude )
where

import TextShow

import Lit
import Resolved

toGitExclude :: Resolved a -> Builder
toGitExclude (RPartiallyApplied _ func args) = undefined
toGitExclude (RList _ xs) = undefined
toGitExclude (RLit _ (LBool True)) = ""
toGitExclude (RLit _ (LBool False)) = "*"
toGitExclude (RLit _ _) = undefined
toGitExclude (RFile _) = undefined

