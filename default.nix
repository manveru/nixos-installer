{stdenv, lib, guile, jq, tzdata, makeWrapper, utillinux
, guile-json
, guile-fibers
, guile-websocket
, callPackage
, elmPackages}:
let
  nixos-installer-frontend = callPackage ./ui {
    elm-make = elmPackages.elm-make;
  };
  makeGuilePaths = drvs: with builtins;
    lib.concatStringsSep " " (map (drv: "${drv}/share/guile/site") drvs);
in
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [
    guile jq tzdata makeWrapper utillinux guile-json guile-fibers
  ];

  src = lib.cleanSource ./.;

  doInstallCheck = true;

  installPhase = ''
    mkdir -p $out/ui
    cp *.scm tests/ lib/ bin/ $out -R
    for i in $(find $out/bin -type f); do
        echo "Wrapping $i ..."
        chmod +x "$i"
        wrapProgram "$i" \
            --set TZDIR ${tzdata}/share/zoneinfo \
            --suffix-each PATH : "${jq}/bin ${guile}/bin ${utillinux}/bin" \
            --suffix-each GUILE_LOAD_PATH : \
                "${makeGuilePaths [ guile-json guile-fibers guile-websocket ]}"
    done
    cp -R ${nixos-installer-frontend}/* $out/ui
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
