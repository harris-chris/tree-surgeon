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

