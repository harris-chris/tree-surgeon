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

## What's the point?

### Bash interop
Continuing the example above, we can then generate a bash array of these files, allowing us to copy these files (and these files only) to a new directory, _maintaining the same directory structure_:
```
$ cd ~/Documents
$ FILES_TO_COPY=$(tree-surgeon to-bash \
    -f 'endsWith ".md" (basename file) | elem "myProjectFiles" (parents file)' \
    -s ./)
$ cp --parents $FILES_TO_COPY /path/to/copy/destination
```

### Nix derivations
When creating nix derivations, the `src` field is hard to set correctly - if it is set too broadly, then unnecessary re-evaluations and re-builds will occur. `tree-surgen` allows you to pick out only the files which directly matter to the build from your source code. Examples of files which are not needed for the build, but which tend to be in the repo anyway and so cause unnecessary re-builds, are:
- version control files, eg the `.git` folder
- documentation
- the nix files themselves
- change logs, developer's notes, and general detritus like vscode config files

To use `tree-surgeon` with Nix, do the following (this assumes you are using nix flakes):

1. Add tree-surgeon to your `flake.nix` - an example here:
```
# flake.nix
{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    tree-surgeon.url = github:harris-chris/tree-surgeon;
  };
  outputs = { self, nixpkgs, tree-surgeon }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      tsLib = tree-surgeon.lib.${system};
      testPackage = pkgs.callPackage ./test-package.nix { inherit tsLib; };
    in {
      packages.${system}.testPackage = testPackage;
    };
}
```

2. Use the `prune` function in your derivation - this is a minimal example, which does not build anything but just copies the source files to the nix store folder:
```
# test-package.nix
{ stdenv
  , tsLib
}:
stdenv.mkDerivation {
  name = "test-package";
  src = tsLib.prune ./src "endsWith '.hpp' (basename file)";
  dontUnpack = true;
  buildPhase = ''
    mkdir $out; cp $src/* $out -r
  '';
}
```
... changing the `./src` folder, and the filter expression `"endsWith '.hpp'..."`, to whatever you like.

## Supported systems
Only `x864_64-linux` at the moment.

