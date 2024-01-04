{ stdenv
  , callPackage
  , treeSurgeon
}:
{
  prune = source: filter:
    callPackage ./makePrunedSrc.nix { inherit stdenv treeSurgeon source filter; };
}

