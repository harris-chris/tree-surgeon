2023-12-29
----------

We have three current issues:
- Error messages probably still not great
- It does not seem to work with "endsWith '.cpp' (basename file)", does using '' " " '' though
- README is incomplete and the documentation doesn't really describe the command-line options.
- Is a little slow to build the src directory because it's re-validating tree-surgeon
  - Maybe move as much stuff out of the src directory as possible?

2023-12-28
----------

Looking mostly finished, not sure about docs.
Are we consistently using that filter output format in the documentation?

2023-12-20
----------

The DirTree has to have a base level, so you need to remove that in `Output.hs`.

The paths-related work could be done within `applyFilter`. Do we want the path that the actual filter sees to be adjustable? Probably not.

If we don't, then this function should probably not be within `TreeFilter.hs` and should be part of `Output.hs`.

The correct way to do the current thing is to have a function that works on DirTree directly, and use that function on the resulting tree

add back in to bash expansion as part of to bash array

the `--type a` option is working fine.

`isDir file` seems not to be, at least for the bash array command

The directory thing is working fine... possibly.
- if you run it on ./, it behaves correctly, ie it does not prepend the name of the current folder.
- if you run it on another directory, eg ./notes, it does prepend notes. I think this is because in `toBashArray'` it is leaving out the `./` folder, when it should probably leave out the source folder.
- do we want it to show folders relative to the current pwd? that's effectively what it's doing at the moment. Or relative to the target dir?
- `tree` does it relative to the current dir, let's aim for that. Right now even that's not working.

2023-12-13
----------

Think about whether `--parents`, `--exclude` need to be command-line arguments or not. Can possibly leave them out.

The best way to deal with this is to have a dirs vs files vs dirs & files option for `to-bash`
Like `find` does, so `--dirs` to show only dirs, or `--files` to show only files, or `--all` is the default. Then you can do `xargs mkdir -p` on the dirs, then `cp --parents` on the files.

Note that this is actually different from the `ancestors` arg, which we probably want as well.

Right now, `toBashArray` is receiving an already-filtered array (which may have the directories removed, and only files).
It is then applying additional filtering on that, with this `length contsFlat'` thing.
So that's the problem.

`to-bash` doesn't seem to work with empty directories

We might need a two-stage pass for this thing:
-

Going to remove writeTree

Get rid of the `ancestor` arg, all it does is say whether directories as well as files should be included and so it is kind of obsolete

It's not really obsolete.

use `cp --parents {test/test-data/a-project/file_a_1.cpp,test/test-data/b-library/file_b_1.cpp} mostunusual` to copy multiple filtered files
You need the `-r` switch to copy across empty directories, but you also need to be careful with this because if you use it on a full directory, it will copy all those files. For example:
`cp --parents -r {test/test-data/a-project/file_a_1.cpp,test/test-data/b-library/file_b_1.cpp,test/test-data/b-library} mostunusual/` will copy all the files in b-library, because you have requested that directory.

`tree` is not displaying correctly

The `tree` binary file in test data is not copying across correctly, I don't think we can rely on the write function, need some way to do it using bash

I think we want to use `AnchoredDirTree` more. This allows us to swap out the root.
The anchor of a tree is one lower than its first level. So
`readDirectory "/home/chris/per/tree-surgeon/test/test-data"`
gives us
`{anchor = "/home/chris/per/tree-surgeon/test", dirTree = Dir {name = "test-data" ...`


2023-11-29
----------

The difference between `((<=) 4)`


2023-11-28
----------

Get rid of `ArgValidation`, `ExpFuncs`.

2023-11-26
----------

showTree' throws an error when it hits a Failed - are we handling `Failed` correctly?
Probably it should take a concrete parameter rather than `DirTree a`, we need a `NonFailed` type most likely.


2023-11-23
----------

New language thing:
- Have a good way to manage repetition. allow repetition but have a tag to say that two sub-expressions are the same. improves readability.

We do *not* want the Exp variables themselves to implement semigroup, because what does it mean to do `LPar <> LList`
Do we even want the Exp variables to implement getTrace? Or do we just want the range variable itself to implement getTrace?
There are two viable options here:
    - the range variables implement getTrace, AND ALSO store the clauses etc and other metadata.
    - the Exp variables implement getTrace (not semigroup)
We should call the `a` variable `IsTrace`, not `HasTrace`. And that should be the restriction. That's much neater.

Note that it would probably simplify the type system a bit if we did have a single type, not `Reproduces` and `IsTrace`.
- The difference really is semigroup. But maybe if we had an `addClause` function and also an `append` function that would be OK.
- I mean append is just semigroup there. It applies to focus a


Should eg `RawApply` capture the function plus arguments? I think it has to. For one thing, if we did not do that, then in trying to reconstruct the original function call, we'd probably not reconstruct it correctly (eg if it had additional whitespace).

`Reproduces` - just recreates some text from the original source text. Implements semigroup because it is a single contiguous piece of source code
`IsTrace` - gives a fuller explanation. Does not implement Semigroup because it may take original source from multiple locations.


2023-11-22
----------

We have two similar typeclasses which are possibliy the same:
`Reproduces` for re-creating the original text, from a Range
`IsTrace`, for

`Reproduces` needs to be a Semigroup, I think. Less obvious how to make

These are probably the same really.

But `Reproduces` recreates only a range. whereas `IsTrace` recreates a snippet, including its sub-clauses.

Should `IsTrace` be on the expression rather than the expression? No I don't think so.
What about for, say `RawApply` though? If it's only on the `a` then it feels like we're storing two parallel hierarchies, the hierarchy within the expression, and the hierarchy within the `a`.

How might that work?
```
error in endsWith someStr someNum
... where someStr was defined here: let someStr = "foo"
... where someNum was defined here: let someNum = 1
```
When printing the trace, we could check if any of the `args` of `RawApply` are outside the Range of the `RawApply` itself. But this feels a bit clumsy.

When printing the trace, do we want to refer only to `RawApply`s main `a`, or the `a`s of the `args`?
- just doing the former feels a bit clumsy
- but doing the latter requires some more sophisticated logic to see if we need to state where clauses.
- Maybe it would be better to have the `a` as `Maybe a` so that we can eliminate them as they're used up. Or maybe we give `Trace` a kind of null constructor. That's better.
- Maybe `Trace` has `focus` and  `additionalClauses` field?
  - So you want to print the trace for `RawApply`:
    - `RawApply` only stores its own trace, ie literally just to the function name.
    - It iterates through its args and `<>`s their `focus` fields to itself, to create
        `endsWith someStr someNum`
    - Then it iterates again through its args and prints their `additionalClauses`s separately:
        `... where someStr was defined here: let someStr = "foo"`.
  - This seems neat, but are there any other additional clauses we can think of?
    - Perhaps `... where (+ 1) is a partially applied argument expecting an additional argument of type Int`.
    - Do we need something like a `define` function? This is probably `errorShow`.
  - Do clauses need to be stored, or can they be magicked up on demand?
    - Stored, I think. Think of `OriginallyDefined`.
  - So we could have a single typeclass for this, it should do `reproduce` as well as semigroup.
  - Semigroup for Trace should string together the focus fields, and stack together the additionalClauses.

Currently RawExp comes out the parser with `a` as X.Range.
We want it to be converted (at some point) to `a` as `Trace X.Range`. We can get rid of `TraceSpan`.
This needs to happen before `deName`, for which we need the whereClauses thing. It should probably happen within the parser. Probably just give everything a simple Trace type.

2023-11-19
----------

Validate the tree first. This means we can have a neater separation between filtering exceptions and other exceptions

It is wrong to say that a simplified RawApply must have a RawIdent as its first argument.
It could have another function that is partialy applied, like +1.
Can we always say that
`RawApply (RawApply ... )`
can be simplified to
`RawApply ...`
What if the function is (incorrectly) fully applied? like
`(1 + 2) 1`
If we simplified this to `RawApply + 1 2 1` then it will give a weird error message, too many arguments applied. Whereas if we resolve it, it gives a good error message ("3 is not a function").
So it does indeed seem better to keep Idents in SimpleExp, then in `simpleExpToResolved`, resolve them all as `partiallyApplied`.

( RawApply
    ( RawApply
        ( RawIdent + ) ( RawLit         1 ) -- correctly formed
    )
    ( RawLit     1 )
)

After RawExp has been simplified, we know that any remaining RawIdents must be in-built function calls.
These RawIdents should always occur at the start of the `RawApply`s, because we have sorted out infixes.
With this in mind, we do not need the `SimpleIdent` type in `SimpleExp`, and instead we can just havea text `name` field for each `SimpleApply`.

2023-11-16
----------

make `prettyShow` parametric so it can be used on both `RawExp` and `ResolvedExp`.
It should use the `ASTShow` `astShow` function to get its thing.
`errorShow` is just for error messages and is something simple, probably v. similar to `show`.

`RawExp` should have `EApplyFlat`, `EIdent`.

`ProcessedExp` or whatever should have `EPartiallyApplied`, no `EIdent`.
Just get rid of `Exp`.

Then errors.

The main issue ATM is exceptions. Having a single file of Exceptions means we can't include Exp, Resolved etc in the Exceptions, because it will create circular imports.
Instead the Exceptions should probably be mostly record types with text fields.
We probably do need an `errorShow` function to get this text from `Resolved/Exp/RawExp` etc.
And the critical thing is the `a` type parameter, right now it's `X.Range` but it should probably be something more like a tree:

2023-11-12
----------

The main issue ATM is exceptions. Having a single file of Exceptions means we can't include Exp, Resolved etc in the Exceptions, because it will create circular imports.
Instead the Exceptions should probably be mostly record types with text fields.
We probably do need an `errorShow` function to get this text from `Resolved/Exp/RawExp` etc.
And the critical thing is the `a` type parameter, right now it's `X.Range` but it should probably be something more like a tree:

```failed: or (endsWith "hs" "this.hs") (1 + 2)
the first argument resolved to type `Bool`
the second argument resolved to type `Integer`
```

```failed: ewhs 1
where ewhs is `endsWith "hs"`
ewhs expected an argument of type string but received an argument of type integer
```


In future we could consider different parsing Stages.
- We can get rid of `EIdent` at an early stage and instead parse into `EPartiallyApplied FunctionDefinition []`.
- We can get rid of `EPar` when we 'removeParens'.
- `Raw` could parse to `EApplyFlat a [Exp a]`, this probably isn't that hard.
- Then `Exp` could just have `EPartiallyApplied` in it, no `EPar`, or `EIdent`, or `EApplyFlat`.

2023-11-08
----------

The function definitions now need to take a separate range argument.

One way around this tricky import problem is to have exceptions defined in each module, and to combine them all all later.

Or we have a special `FunctionArgument` Type, which is `Lit, PartiallyApplied, EList,`. Not sure about this idea though. Actually this might not be bad. This would effectively be a list of the program types.
Then have `expToFunctionArg`. And don't convert the first argument to `PartiallyApplied`, just immediately `applyFunc` it.

We would map Exp to Resolved as follows:
EPar -> N/A
EApplyFlat -> FPartiallyApplied
EList -> FList [Resolved]
ELit -> FLit
EFile -> FFile
EIdent -> N/A

And `ArgValidation` could just be `Resolved`.

errorShow (EPar _ x) = "( " <> errorShow x <> ")"
errorShow (EApplyFlat _ xs) = T.intercalate " " $ errorShow <$> xs
errorShow (EList _ xs) = "[ " <> (T.intercalate " " $ errorShow <$> xs) <> "]"
errorShow (ELit _ x) = LT.errorShow x
errorShow (EFile _) = "file"
errorShow (EIdent _ (VarName _ name)) = name

Or we have EPartiallyApplied in `Exp a`, but we match it to its function much later. That would keep its shape simple.

Currently args are [Exp a], and this does not include PartiallyApplied.
This might be OK if we resolve EApplyFlat within `resolveFunc`, therefore PartiallyApplied is only ever an intermediate type.

We are getting a circular import. This highlights an irregularity whereby EPartiallyApplied is not really a member of `Exp`. Is there some way to not have it there?
What if `resolveFunc` resolved to its own `Func` type, containing EApplyFlat and EPartiallyApplied?

2023-11-08
----------

We need an additional way of show-ing the Exp a, for error messages. We don't want to `prettyShow`.

As a starting point to fix this, `deName` should put the substitution in parens. I'm not sure this would fix it, though, because parens are being removed.

basenmae file is resolving to EPartiallyApplied basename file, should just resolve
Note that at the point that `resolveFunc` is called it's doing it wrong, the `EApplyFlat` has three arguments total.

There's probably a better way of doing `resolveFunc`, such that it converts all functions to partially applied functions and folds up.



If a partially applied function is used within an `EApplyFlat`, there is no way to know that it is a function rather than an identifier.
For example, in `map ! [True, True]`, `!` is assumed to be an identifier.
Do we want a function like `resolveInfixes` that corrects these? or do we want to do that within `resolve`? Currently it's not doing this correctly, although it feels like it should be, inline 67 of `FunctionType.hs`.


2023-11-07
----------

let x = 1 in x seems to be working OK in tests but not for some reason from command line.

Do we want ot get rid of `FileFunctionNotAppliedToFile` and use `ResolvedumentWrongType` instead? It would probably mean parsing file in the AST so maybe not.

Actually I think it could just have its own `ArgValidation`.



2023-11-07
----------

The "I can only filter directories" error is shown even when there's not a file, eg if something has been specified that doesn't exist.

I think that EApplyFlat's range variable is being lost somewhere in `resolveInfixes`.
Need to put it in acc I think.




2023-11-05
----------

I think the recursion needs to work base-to-leaf. Eg
`EApplyFlat basename file == "file_a" | basename file == "file_b"`
first of all it goes through and tries to process basename, file, == etc.
then it goes through the base level
`EApplyFlat | (EApplyFlat basename file == "file_a") (EApplyFlat basename file == "file_b")`
The leaves have already been processed at this point so it's not going to go back to do them again.
Instead work base-to-leaf

In a bit of a chaotic state right now.
- something wrong with resolveInfixesForOperator'.
- resolveInfixes does not seem to be working recursively, for some reason. it only finds infixes at the top level.
-

Having some trouble thinking through the various cases with infixes:
- If a function name has been given in parens, it will be parsed as an EIdent
`1 + 2` => EA

With `traversable` now implemented, we can probably remove the parentheses at a late stage, as they're easier to work around.

I actually think it's foldable that matters to us. `Functor` for example can't change the type, it goes `Exp a` -> `Exp b`, but the `Exp` has to be the same variant in both cases.

Either that, or we could leave them out of Exp entirely, but that's out of synch with the current approach.

2023-11-04
----------

ResolvedumentWrongType should take the right type (not a `prettyShow badArg` as its parameter).

Then we want to refactor to implement a lot of functions - `parseApply`, `removeParens`, `parseOperator` to use `Traversable`. Which is now implemented for `Exp a`.

I think we need to do some refactoring. Some points:
- Removing parentheses is quite tricky. It can't be done until infixes have been parsed, for example. It should almost be done last.
- parseFunctionCalls and parseApply are basically the same function.
- I think a lot of this would be easier if traversable was implemented for `Exp a`.

Just ! is not treated as a partially applied operator. It gets parsed as an EIdent.

parseApplyOperator is stopping at the first level if it doesn't find the operator. So one operator within another won't work.



Should we:
- parseApplyOperator for all sub-EApplyFlat?
- I think it's important that we do one operator then another,
- so `parseApplyOperator &` to all levels, then `parseApplyOperator |` etcetera.
- This should already be happening, because `parseApplyOperator` should iterate through, and it has a match on `EApplyFlat`

2023-10-29
----------

Have removed all `Eq a`, `Show a`. Think that's the right thing to do. Follow the errors from here on in. If Show is needed for eg `Exp`, do not show `a` within it.


Neither Exp a or Raw a should need to implement Show a. `a` is just the range.

The tests are almost running, I think. The main issue is that the type constraints are a bit of a mess. I think we should expect Exp and RawExp to implement `Show`. And the original token needs to implement `Semigroup` and `Reproduces`.


2023-10-18
----------

Bear in mind that the Lexer doesn't know that `Let x = 1 in x` is a single expression, it just sees `LET`, `x`, `=` etcetera. So it can't store entire expressions. We probably want to re-assemble the complete expression text within the parsing. It must be within the parsing that the semigroup instance is being used.

Another approach here would be to *not* have Range store text, and have all TSExceptions return functions that take the original filterStr.

The semigroup thing isn't going to work anyway, because the lexer will have eliminated some of the whitespace.

! One trouble with the ideas above is that `parseError` wants to return an alexError, which may have to be text? Why does it want to return an `alexError`? It's being called from `Happy`.

Is it the semigroup instance of Range that's creating these strange agglomerations? Can't really see how since that's only being used downstream. Actually it is indeed this that is causing the problem. Probably `mkRange` is wrong.

It seems that `mkRange` is not being called for every token (eg, not for False, True or Ident), this might be the problem. It should be being called, but it's not.

2023-10-17
----------

Add id function
Things to error check
- brackets get opened but not closed

2023-10-16
----------

An EApplyFlat can start with a function name, but can also start with a partially applied function.
With `resolveFunc`, why don't we:
  - check what the first Exp is, and reject unless it's EIdent or EPartiallyApplied
If a partially applied function, probably best to use

There is something nasty and recursive here.


Possibly `parseApply` or whatever should be the thing that reduces down the arguments.

`resolveFunc` doesn't do anything if the first `Exp a` is not an `EIdent`.
But what if it's a partially applied function?


2023-10-15
----------

Have separated out RawExp from Exp. Partial application still remaining to be implemented.

Have the types specify the list type, so not just `EList a xs` but `EList a ArgValidation xs`? Sounds a bit like static typing and not sure where you would do this - the rawExp -> Exp stage?
Actually yes. then.

2023-10-08
----------

only EApplyFlat should have `resolveFunc` applied to it, there's no sense in doing so for `EPartiallyApplied` because we know it can't resolve
However EApplyFlat can resolve to EPartiallyApplied

Then we need a new function, to be used only within `Functions.hs`, like `resolvePartiallyApplied` which passes the required number of arguments to it. Or applies the arguments one at a time and returns either EPartiallyApplied or some other Exp a.

I think we need a two-step process, applied recursively:
only
EApplyFlat -> either EApplyFlat or EPartiallyApplied
then

I don't think we need to record how many arguments a partially-applied function still needs, this can be checked every time in `resolveFunc`.
We do need to store the arguments it already has, I think. But this depends how it's being called.
Though, some functions do need a way to say, I expect argument 1 to be a function that needs one remaining argument.
So, for this hypothetical argument 1:
  - it has to have a store of what arguments it has already received
  - it needs to know what it still expects
I think we need a new Exp type, `EPartiallyApplied a [Exp a] Int`

We might want `Exp a == Exp a` to return an error rather than False if they're not the same type

A RangedToken is `{ rtToken :: Token, rtRange :: Range }`
the `a` in `Exp a` is just `Range`

We need to make `a` a semigroup so that we can use `<>` on it. Right now `Parser.y` gives it a `<->` function.

2023-10-06
----------

About ! True - what to do with this?
Do we want a kind of 'close parse' so that !True is OK? Probably not right?

2023-10-05
----------

We want ideally a generic way to argsParse, a function which takes the `argParse` function and throws up the usual errors.
We could have the `Function` args as a list of checks, each corresponding to an AST type. Like `checkIsAnd`, etc.
Then we:
- check that the list length matches
  - if args < expectedArgs, return a partial application
  - if args > expectedArgs, return an exception
- check that the arg types match
  - if not then throw exceptions


2023-10-04
----------

We want AST types for functions which are depth-typed, I think. Or we want a record type for defining functions, and `resolveFunc` returns an `a`, or an `a -> a`, or an `a -> a-> a`, or whatever. I think we have to resolve the function before we can know this.

For the current problem, I think we just need `parseApply` or `resolveFunc` to remove `EPar`s.
Remember that EPar is also used as an indicator of prefixity. So probably this should be done in `parseApply`.
Is EPar useful at all beyond parse-time?



2023-10-03
----------


We probably do want to use that x -> (y -> z) style for functions, it seems to be widespread. Probably possible to write `parseApplyOperator` in that style. And we'd want to make it an instance of functor, traversable.

Haskell does not have the `partiallyAppliedFunction` AST type, it probably just has
`(a -> a)`
`(a -> a -> a)` etc
I guess these are defined using a type constructor. Are these function applications?

Could you ever parse to `(a -> a)`?
I don't think so, because at parse-time we don't know the number of arguments that a function takes.




2023-10-01
----------

doesn't seem to be running basename, other than that somewhat improved

infix parsing seems to be working now, the issue might be in functions

If `==` is being used as a function, rather than infix, how do we parse that? What should we do? Just leave it where it is?
This is within an EApplyFlat

in Haskell the infix operators can have () around them to make them prefix, so
`(foldl (+) 0) [1..5]`

So we shall do things that way.

so `map + 1`, should this be `+ map 1`

If you look at the parsed expression for the only active test, it does not re-arrange it correctly - the `==` is still infix.
So parseApply going wrong somehow.

2023-09-29
----------

flattenApply is not penetrating to the deeper levels of an EApply. There are still EApplys present after it has been used.
The problem is still there in line 118 of ASTFuncs, we probably just need a single function that we apply like parseFunctionCalls
could do it via double recursion but it's a bit ugly

We could have a special parse for special functions at the start of an exp, like `!` and `-`.

parseApply should only be run once, it should probably be in the same function as deName

We are parsing function applicatin to (EApply x (EApply y (EApply z z')))
but then treating it as EApply [Exp a]
Why don't we just parse it to EApply [Exp a]?
I can't remember what the original reason was for changing this.

2023-09-27
----------

I think we need a `factorOutInfixes` function that goes `Exp a -> [Exp a]`

How to handle these tricky parses like:
basename file == "this"
basename file == basename file

I think we have to go through the Exp stack, processing the *lowest* precedence operators first.
So `basename file == basename file` => `(basename file) == (basename file)`
Is this only ever relevant for infix operators? I think it is
`1 + 1 == 2 * 1`
=>
If so then maybe we do start from the highest precedence, and work through:
`(1 + 1) == (2 * 1)`

Just do multiple passes through, one for each operator, starting with highest precedence
What if multiple operators exist in the same?
If we do this, then we also have to assign a high precedence for normal function application,
or else `basename file == \"myfile\"` would become `basename (file == \"myfile\")`

This is quite tricky, and the kind of thing that the parser would usually do. But it won't in our case, but these functions do not exist at the parser level.
It might actually be best to start from the lowest precedence and move up

2023-09-26
----------

Should `deName` replace all `EIdent`s?
`deName` should replace the function `EIdent`'s with `EApply`, eg
`basename file` ->
`EApply (EIdent basename) (LFile)`
actually not sure what I'm saying
maybe we need a special carve-out list of names that don't get `deName`-d, including `basename`
`deName` just substitutes `Exp`s.
`resolve` does `Exp a -> Exp a`
Maybe these are the same thing
We could have a `FunctionAppl` type that does `[Exp a] -> Exp a`. Possibly we could replace
`EApply (EIdent basename) (LFile)` with `EApplied`.
We'd always end up substituting function names for other function names, so this makes no sense.

But how would we represent

2023-09-25
----------

Why can't we deName with `file`, and replace it with an FData Exp type, EFData?


2023-09-25
----------

Had a long think about this at the airport yesterday. Conclusions were:
- We want to record at the parse level whether an argument is fully applied or not.
- Parse functions as their own functions, rather than just as a generic function (probably)
- But parse them

2023-09-23
----------
You have just converted EFunc to LFunc, so that it can fit in LPar. This feels like a  correct decision.
Now you need `deNameLit`. `resolveExp`, `resolveLit`, `deNameExp`, `deNameLit`
Hard to know how to parse infix functions.

More notes on this on the remarkable.

2023-09-20
----------

Expected and actual are the wrong way around in the tests!

What's the desired behaviour? remove empty folders by default?
Or make the user write that? Could be a command line option but don't really like that.
How would it be written?
It's more like that the default is to keep folders if they have files in (even if the filter doesn't include them)
so the behaviour should be:
- they get assessed like everything else, they don't have an in-built `FData` but one can be constructed
- if they're not included, but files in them are included, they get kept (this is the exception)
- otherwise like files they get cut

The next challenge is to get filterTreeWith' working in a monadic way.
Make sure the initial directory doesn't filter itself away.
A directory with included contents cannot be filtered away(?)


All function arguments should resolve to Lits (everything should resolve to a Lit).
Probably easiest done recursive rather than in stages. So `EFunc [Exp a]`, but within the resolveFunc function, turns `Exp a` to `Lit`.

These problems again
- Can we say that everything resolves to Lit if given `fData`? Probably we can
So:
- Make "Or", "And" functions, not language features
- Still have a `simplifyExpr` function, which does `deName`, `dePar`.
- Have a resolveToLit function, `FData -> Exp a -> Either RuntimeException (Lit a)`
- Then at the final stage check that that lit is a bool

I think we need a special place for functions that take `FData`. `FData` may need its own Parser object. The final type is `FData -> Either TSException Bool`

So we have some preliminary functions for simplifying the expression, of the type
`Exp a -> Either ExprException (Exp a)`
such as
`deName` but also `dePar`!!!
These are mostly here not to speed things up but to catch grammar errors.

Then we have a `resolve` function that is
`resolve :: Exp a -> FData -> Either RuntimeException Bool`

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

