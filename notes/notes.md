We need a not statement!
What is the relationship between using not and using exclude? Can we draw any firm conclusions?

With the exclude function, we only want to output stuff that's been excluded. Excluded means:
- Any files that are specifically exluded
- Any directories that are excluded as a result of having no files in them.

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
