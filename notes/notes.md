Notes on where we currently are:
- Read the stuff directly below, it's useful
- We need to move the AST to its own file, because both Functions.hs and deFunc need it, and deFunc depends upon Functions.hs.
- All going well


Either we:
- Have special syntax for the variable name, `file` or `row` or whatever we're calling it, and replace that special syntax with the real variable at some point. This variable has a special type (it is not a string). This would not get replaced during the `deName` stage, but would survive until the `deFunc` stage and get replaced with strings or whatever else then.
- in deFuncs, we have some built-in functions, like `basename`. Right now we have no syntax to create functions, but this is something we want to add. So if the user has done:
    `let nameMatchesMyFile = \a b -> (==) "myFile" b in nameMatchesMyFile (basename file)`
    or `let nameMatchesMyFile = ((==) "myFile") in nameMatchesMyFile (basename file)`
    we want this to deName to
    `(==) (basename file) "myFile"`
    so we need the parser to recognize that `nameMatchesMyFile (basename file)` in the original is a function, which it should already do
    but to do the let replacement, we probably need some special case, which combines the two recognized functions (`(==) "myFile"` and `nameMatchesMyFile (basename file)`). Something like:
  `Let [("nameMatchesMyFile", EFunc _ "==" ["myFile"])] (EFunc _ "nameMatchesMyFile" [(basename file)]`
  `deName`s to `EFunc _ "==" ["myFile", (basename file)]`
  which then `deFuncs` to `EFunc _ "==" ["myFile", "file_a.cpp"]`
  and then `deFuncs` again to `False`.
  So the conclusion of all this is that:
    - A variable name is just a special case of a function that has no arguments
    - we need Bools in the AST.
    - we need a special case of `deName` to deal with this (we now have this)
    - `==` needs to be a function, not a piece of AST syntax.
    - Within the parser, we can make a function infix if we want to
    - We need to supply the built-in functions (eg `==` to `deName`)
    - We need to inject the `file` variable at the deFunc stage.
-
-

This system doesn't work, because these stages can be nested within one another.
Only stuff that can't contain other stuff can be done like this, like literals. A Func can contain a `StageAExp` as one of its arguments, so you'd need to have `StageAExp` as its type. I also worry that this will generate an insane syntax tree.

Could this be done with parametric types? So types like `Par` are parenthesized initially with a union type containing everything, then after `resolveNames`, they are parenthesized with a union type containing everything minus names.

(Names + Lets) + Values (includes Literals) + Booleans + Logicals (includes False/True)
-> `resolveNames` ->
Values + Booleans + Logicals
-> `resolveLitFuncs` ->
Literals + Booleans + Logicals
-> `resolveBoolFuncs` ->
Logicals (And/Or/Not/Parentheses/False/True)

Logicals + Bools + (Literal == Literal)
-> `resolveEqualities` ->
Logicals + Bools
-> `resolveLogicals` ->
Bool

collectively `resolveToBool`

So a three-stage process. Could probably combine stages 2 & 3.

Syntax
  which has `resolve` so
    - Let
Resolvable (eg Or, Eqs, here Eqs takes Values not Exps)
Value

Funcs resolve to a value. A value maybe a Bool.
All Exps resolve to a Bool.
`name row` is of type `String`

where Value does not resolve to Bool but Resolvable does.
do we though? Maybe we just need to simplify everything down to Values.

In some way, isn't all matching involving the comparison operators? What would not be? All matchers have to return boolean. What about using eg any though? Or startsWith?
`any (startsWith "file") files`

Some functions are matchers and some are not.
- `name` is not a matcher, eg `name path`
- `nameIs` is a matcher, but maybe == should be the only way of matching?

We could either have a `Matcher` type that can do `getMatcher`, or have a `resolve` function that's like `deName` but also resolves non-matcher functions like `name`. That second one feels easiest.
Do we want to consider eg `==` as a function? We probably wouldn't want to have it prefix. But it should be possible to have it infix.

If we allow people to define custom functions via lambdas and assignments:
`let startsWithFile = \x -> startsWith "file" in startsWithFile str`
then we have to have a generic `func` exp, rather than separate AST objects for different functions.

If we have a `func` exp, then it has to take a list of arguments, of unknown number. This makes parsing quite hard, you then have to have a way of knowing when you've hit the next valid expression. But maybe actually it's not.

We are proposing to have a `func` exp, which takes any number of arguments.

- Currently all the matcher `Exp`s do not take an argument, it might be more obvious what to do if they did. At the AST stage we are just resolving the grammar, so the matcher expressions should resolve to `Matcher VarName`.
- The `Exp` type might need to have `getMatcher
- Have a special AST object, `File`, for which `getMatcher` gives a `Matcher` that looks takes the FsObjData and

We could have:
- a function which has a single parameter, a list of directories, with the file (note that this may be a directory) as its final object. Have `basename` and `dirnames` functions which effectively are just `last` and `init`. The advantage of this is that we can have a single set of functions, eg `nameIs`, not `ancestorNameIs`. What would we call this object? `row`? Have a `type` function to tell if it's a file or a directory. Make sure these names are consistent with unix convention.
- or, the argument could be the actual full string, eg `this/that/my_file.cpp`. Then `basename` gives you `my_file.cpp`.
- Then if we also had partial application
  - `nameIs "file_a.cpp" (basename row) & any (nameIs "a-project") (dirnames row)` to capture `file_a.cpp` in any folder which is, or has ancestor, `a-project`.
  - `nameIs "file_a.cpp" (basename row) & ds == [ "a-project" ]` to capture `a-project/file_a.cpp`.
  - `nameIs "file_a.cpp" f & endsWith [ "a-project" "docs" ] ds` to capture `**/a-project/docs/file_a.cpp`.
  - `nameIs "file_a.cpp" f & startsWith [ "a-project" ] ds` to capture `a-project/**/file_a.cpp`.

Have `is`, `contains`, `endsWith`, `startsWith`
We have two options:
  - `name` to get the name of `f` and `ds`, or
  - we could have additional parameters, eg `fileName`, `dirName`, `fileData` and `dirsData`
Realistically we might never get beyond `name`, so maybe best to use the second
The syntax for the first would be something like:
  - `is "file_a.cpp" (name f) & any (\d -> is "a-project" (name d)) ds`

in Haskell:
`any (\d -> isPrefixOf "ab" (stringVar d)) lstA` works fine, but not
`any (isPrefixOf "ab" $ stringVar) lstA`, which doesn't work

so we would need lambda functions and (ideally) lambda functions for this to work.


The current error message is correct. Using `traverse`, we can change the type parameter but not the hshape or overall parameter. So we can't go from `Exp a` -> `Either TreeSurgeonException (Exp a)`.
We could go from `Exp a -> Exp (a, Maybe TreeSurgeonException)`.

It might be that foldable is the superior monad to use here. but does it preserve shape?
it seems that `toList = foldr (:) []` so no it does not preserve shape.

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

