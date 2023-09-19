I think we need a special place for functions that take `FData`. `FData` may need its own Parser object. The final type is `FData -> Either TSException Bool`

Can we totally re-think this?
- There is only an `LFunc`, it only ever resolves to a `Lit`.
- At the end we assess to see if `ELit` is a bool.
- Either we can do that, which would simplify things, or the current approach where we convert (`FData -> Lit a)` to `(FData -> Bool)` won't work
- It should be possible to know at parse-time if `(FData -> Lit a)` can be converted to `(FData -> Bool)`. In which case:
  - Just need LFunc, not EFunc
  - ELet just needs to be able to define Lit, not Exp
    - `And`, `Not`, `Or` could be functions not Exp. That way you can still do
      - `let invertMatcher = Not in invertMatcher (basename file == "myFile")`
  - Sounds like we might be back to having a single type. This is all on the assumption that we can know at parse-time whether a combination of `Exp`s will resolve to a Bool. I think that's possible.

We need to be organized about where `deName`, `resolveLit` etc are called from.

- `deNameExp` needs to call `deNameLit`.
- should `resolveLit` be called from `getMatcher`? Would probably be fine.
- or we could have a separate `resolve` phase
- then call getMatcher.


I'm not sure we need EVar/LVar, just EFunc/LFunc


*Dealing with Let*
Let has to be part of Exp, not least because it may exist at the top level, eg `let x = True in x`
But you could also have `let x = "myFile" in (basename file) == x`, where the variable itself is a lit.
And you could have `let x = basename file in takePrefix x`, and here the `let` expression itself resolve to a `Lit`. So I think we need both kinds of Let.
The problem with this is that the ELet needs to be able to take NamedExp and NamedLit, ie it needs to be able to handle:
  - `let x = "myFile" in (basename file) == x`
  - `let x = True in x`
I think we assume that `Let` exists at the `Exp` level, but can only take `NamedLit`. Then we make `let x = True in x` a special case, somehow. Though it is difficult to know how to do this. We could have an `EPureLit` type, but then we wouldn't know that it resolve to a bool at parse-time. Or maybe we would? if you `deNameExp` (or `resolve`) an `EPureLit`, then you could throw an error if it resolve to anything other than `LBool`. This is fine, but then how would we resolve:
  - `let x = (== (basename file) "myFile") in x
Because this is a `Let [NamedExp a] (Exp a)`
Can ELit work here? Think it can. We convert `Exp a` to `Lit a` if we encounter an `LVar` when applying `deNameLit`.

We probably need to treat `Bool` as an `EFunc`.
We could make `Let` part of `Exp`, but have `deName :: Lit a ->...` which is called by `deName :: Exp a ->...`

EFuncs:
`==`
`True`
`False`

LFuncs:
`basename`

When would we want an LFunc to resolve to Bool? It's possible but I think we can add it in later, LBool can co-exist with EBool.


The `file` object itself can be thought of as having no `Eq` instance, and so should only ever be part of function arguments.

Notes on where we currently are:
- Read the stuff directly below, it's useful
- We want the `Either TSException` bits to be during parsing the expression. By the time it comes to applying it, it should not be in an `Either`.

`deName` just simplifies the `Exp` and has no relationship to the `file` object, so this can be run at any time. The tricky part is to have a bit of the process that goes from `Exp a` to `(FsObjData ->  Bool)`. This probably needs to be in the `deFunc` stage, where (non-Let-related) names are resolved.
We possibly need a two-stage process:
`deFuncA` goes `Exp a -> Either TSException (FData -> Exp a)` and resolves the functions that return literals. The key thing here is that the functions that don't return bools, like `basename`, have to be arguments to other functions that do take bools. So as an expression,
`basename file` is no good, but
`basename file == "myFile"` is good.
- All functions that resolve to literals, rather than bools, should be arguments to other (resolving-to-bool) functions. Resolving these is the stage which requires FData to be injected.
- Perhaps the AST itself should have two types - `Exp`, which resolves to Bool, and `Lit` for anything which is a literal or resolves to a literal.
- `EString`, `EList` are clearly literals, they cannot say yes/no by themselves.
- `EFunc` is the ambiguous one, since it will have both resolves-to-bool and resolves-to-list functions.
  - We could assume that all Exp-level functions resolve to bool, and that all functions that are actually args to Exp-level functions resolve to literal (where literal could include the Bool type). This seems robust. The resolves-to-lit functions will take the `fData` argument, and then the Exp-level functions also need to take this in order to pass it through to their argument functions.
- So either exp-level or argument-to-exp-level functions may return the Bool literal, and it is the Bool literal (not the actual Haskell Bool type) that an Exp resolves to.
- I think this is the approach that we have to take, but at first glance we can't say at the type level that a matcher will resolve to `EBool` (versus any other Exp constructor), and so we can't say that the matcher will definitely resolve to a bool, which is really what we want.

`deFuncB` deals with functions that don't relate to `file`, like `==`.
or perhaps we have a single-stage process and just don't actually use the `file` argument in functions like `==`. Hard to resolve them otherwise, unless we replace them with new `Exp` constructors, like `Eqs`.

`deFunc` should reduce the expression to
- Logical operators
- Built-in functions.

*Functions*
Currently you can partially apply built-in functions, you can't create your own.

the original matcher looked like this:
type Matcher = (FileName -> FsObjData -> Bool)
type MatcherE a = Either TreeSurgeonException Matcher

So this means we can have errors reading the expression, but not at run-time.
It would be hard to achieve that in the new structure without having a multi-stage type system, where the types are different as we pass from `deName` to `deFunc` to `resolve`. We could probably have a single break:
`deName` obvs needs to return `Either TSException (Exp a)`
`deFunc`, this sounds hard with.:b

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

