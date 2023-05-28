What if you want to get rid of all folders named ".cache"? It's easy to empty those folders but not to remove the folders themselves.

Could have an example where you search then xargs then sed in-place.
Similar to `find`, could have a test where `find` output is compared to this.
Could have smart-casing for strings
Could have different filter string for directories.

We probably want some more applyFilterWith functions:
- apply two filters and compare
- apply inverse filter and compare
These are probably test-only functions.

What is the relationship between using not and using exclude? Can we draw any firm conclusions?

With the exclude function, we only want to output stuff that's been excluded. Excluded means:
- Any files that are specifically exluded
- Any directories that are excluded as a result of having no files in them.

An exclusion cannot result in a tree, it needs to result in either a list of files, like [String], or a datastructure which contains the original and the filtered.

We may need to arrive at the list of inverses by comparison:
- toBashArray the original and the filtered
- An exclude is anything that isn't in the original. This could be done as a function to applyFilterWithComparative

- Use `shelltestrunner` to test the actual output
- Ideally also test executable in combination with shell commands such as xargs

This exlude matching, it will correctly find all files that we want to exclude, that bit's fine
But it's then difficult to filter out directories, they will contain a list of exlusions.
We need to *include* a directory if its `length contents - length exludedContents` == 0
But what about its children?
Possibly you can't really have a DirTree of inverses.
