{ stdenv
  , tsLib
}:

stdenv.mkDerivation {
  name = "tstest";
  src = tsLib.prune ./. "endsWith '.cpp' (basename file)";
  dontUnpack = true;
  buildPhase = ''
    mkdir $out
    cp $src/* $out -r
  '';
}
