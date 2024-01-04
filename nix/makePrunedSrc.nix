{ stdenv
  , treeSurgeon
  , source
  , filter
  , glibcLocales
  , lib
}:
stdenv.mkDerivation {
  name = "pruned_src";
  src = source;
  dontUnpack = true;
  LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
  buildPhase = ''
    export LANG="en_US.UTF-8"
    mkdir $out
    if $(${treeSurgeon}/bin/tree-surgeon to-bash \
        -f ${lib.strings.escapeNixString filter} \
        -s ${source} > call_out 2> call_err);
    then
        BASHARR="$(cat call_out)"
        cd ${source}
        cp --parents $BASHARR $out/
    else
        ERRMSG=$(cat call_err)
        echo "$ERRMSG"
        exit 1
    fi
  '';
}
