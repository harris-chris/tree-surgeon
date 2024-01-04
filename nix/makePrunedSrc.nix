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
  LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
  unpackPhase = ''
    export LANG="en_US.UTF-8"
    mkdir -p $out
    chmod -R 777 $out
    if $(${treeSurgeon}/bin/tree-surgeon to-bash \
        -f ${lib.strings.escapeNixString filter} \
        -s $src > call_out 2> call_err);
    then
        BASHARR="$(cat call_out)"
        cp -r $src/* ./
        chmod -R 755 ./
        cp --parents $BASHARR $out/
        chmod 755 $out
    else
        ERRMSG=$(cat call_err)
        echo "$ERRMSG"
        exit 1
    fi
  '';
  buildPhase = "true";
}
