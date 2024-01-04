{ stdenv
  , tsLib
}:

stdenv.mkDerivation {
  name = "tstest";
  src = tsLib.prune ./src/test/test-data "endsWith '.cpp' (basename file)";
  dontUnpack = true;
  buildPhase = ''
    mkdir $out
    cp $src/* $out -r
  '';
}
