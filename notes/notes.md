I think both namedExprs and Exp needs to be part of the state.
Or else how else do you pass in `Exp`? No, you just have the `runState` function take the parameter

So perhaps we want `runState`, then call `traverse` within `runState`:
runState is of type
`runState do-block initial`
within `do-block` we would call `deNameF`, a non-monadic function. We would need to call it using `traverse` or something similar.

mapM operates on the actual type parameter, ie `L.Range`. this isn't what we want, because we want to alter `Exp`'s non-paramaterized attributes

I think you need to run the traverse in runState, rather than vice-versa. Otherwise `get` and `put` should logically not be available. But not quite sure.

The state monad is like, you run your functions within it, eg `runState (myFunc someParam)`, and then within that function you can get `get` and `put` to alter the state.
This is kind of neat, not yet sure how it combines with `traverse`.

traverse is expecting a function `a -> f b`
so `a` is `Exp a`
`b` is `Either TreeSurgeonException (Exp a)`
so `f b` is `State [NamedExpr] (Either TreeSurgeonException (Exp a))`

traverse can't affect the structure. So we can't collapse away `Let` expressions. We could just resolve the names within them and then ignore them from then on in.
Or we could have a second pass using `foldable` that gets rid of them, I think.

`Traversable` is now implemented for `Exp`. Use this to apply deName.

for us to apply `deName` via traverse, I think we have to apply it like:
`Exp a` -> `Exp [((VarName a) (Expr a))]`
where `f` is of type `a -> [nameDefs]`

We want to have `let x=y in z` and `let a=b; c=d in a=c` as expressions, but by the time that we get to the getMatcher function, we want them to be gone.
So we need getMatcher (Dec a) to error.

So we have a deName function, which removes all the lets, and then a let throws an error in the getMatcher function.

Could we re-write this whole thing using Morte?

What if you want to get rid of all folders named ".cache"? It's easy to empty those folders but not to remove the folders themselves.

Desired examples:
- Copy a filtered directory via ts rather than bash.
Could have an example where you search then xargs then sed in-place.
Similar to `find`, could have a test where `find` output is compared to this.

Nice features:
Could have smart-casing for strings
Could have different filter string for directories.
.gitignore output

We probably want some more applyFilterWith functions:
- apply two filters and compare
- apply inverse filter and compare
These are probably test-only functions.

What is the relationship between using not and using exclude? Can we draw any firm conclusions?

With the exclude function, we only want to output stuff that's been excluded. Excluded means:
- Any files that are specifically exluded
- Any directories that are excluded as a result of having no files in them.

An exclusion cannot result in a tree, it needs to result in either a list of files, like [String], or a datastructure which contains the original and the filtered.

