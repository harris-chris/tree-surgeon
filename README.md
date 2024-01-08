![Documentation](https://github.com/harris-chris/tree-surgeon/actions/workflows/Documentation.yml/badge.svg)

View [the full documentation](https://harris-chris.github.io/tree-surgeon/)

# Tree surgeon

*A mini-language for pruning your directory trees*

Tree surgeon is a tool for applying complex (or simple!) filters to directory trees. For example:
```
$ tree-surgeon tree-diff -f 'endsWith ".md" (basename file)' -s ~/Documents
```
will show a before & after of your `~/Downloads` folder, if all but the markdown (`*.md`) files were filtered out.

Then:
```
$ tree-surgeon tree-diff \
    -f 'endsWith ".md" (basename file) | elem "myProjectFiles" (parents file)' \
    -s ~/Documents
```
will show the same output, _plus_ any files that are holdings (at any level) of the `myProjectFiles` folder (thanks to the `|` logical or operator).

See the [language tour](https://harris-chris.github.io/tree-surgeon/#_language_tour) for more information about how to write filter expressions.

To see a list of available commands:
```
$ tree-surgeon --help
```

## Supported systems
Only `x864_64-linux` at the moment.

