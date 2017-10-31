{stdenv, lib, guile, jq, tzdata, makeWrapper, utillinux, guile-json, callPackage, elmPackages}:
let
  nixos-installer-frontend = callPackage ./ui {
    elm-make = elmPackages.elm-make;
  };
in
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [ guile jq tzdata makeWrapper utillinux guile-json ];

  src = lib.cleanSource ./.;

  doInstallCheck = true;

  installPhase = ''
    mkdir -p $out
    cp *.scm tests/ lib/ bin/ $out -R
    wrapProgram $out/bin/* \
        --set TZDIR ${tzdata}/share/zoneinfo \
        --suffix-each PATH : "${jq}/bin ${guile}/bin ${utillinux}/bin" \
        --suffix GUILE_LOAD_PATH : ${guile-json}/share/guile/site
    cp -R ${nixos-installer-frontend}/* $out
  '';

  installCheckPhase = ''
    cd $out
    set +e
    export TZDIR=${tzdata}/share/zoneinfo
    guile --no-auto-compile -s tests.scm
    STATUS=$?
    set -e
    if [ $STATUS -ne 0 ]; then
        cat *-test.log
    fi
    echo "Test exit code: $STATUS"
    return $STATUS
  '';
}
