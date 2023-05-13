
- Use `shelltestrunner` to test the actual output
- Ideally also test executable in combination with shell commands such as xargs

This exlude matching, it will correctly find all files that we want to exclude, that bit's fine
But it's then difficult to filter out directories, they will contain a list of exlusions.
We need to *include* a directory if its `length contents - length exludedContents` == 0
But what about its children?
Possibly you can't really have a DirTree of inverses.
We may need to arrive at the list of inverses by comparison:
- Flatten the original and the filtered
- An exclude is anything that isn't in the original. This could be done as a function to applyFilterWithComparative
