{stdenv, lib, jq, tzdata, makeWrapper, utillinux
, callPackage
, elmPackages}:
let
  nixos-installer-frontend = callPackage ./nix/ui.nix {
    elm-make = elmPackages.elm-make;
  };
in
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [
    jq tzdata makeWrapper utillinux
  ];

  src = lib.cleanSource ./.;

  doInstallCheck = true;

  installPhase = ''
    mkdir -p $out/ui
    cp *.scm template/ tests/ lib/ bin/ $out -R
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
