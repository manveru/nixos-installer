{stdenv, lib, guile, jq, tzdata, makeWrapper, utillinux }:
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [ guile jq tzdata makeWrapper utillinux];

  src = lib.cleanSource ./.;

  doInstallCheck = true;

  installPhase = ''
    mkdir -p $out
    cp *.scm tests/ lib/ bin/ $out -R
    wrapProgram $out/bin/* \
        --set TZDIR ${tzdata}/share/zoneinfo \
        --suffix-each PATH : "${jq}/bin ${guile}/bin ${utillinux}/bin"
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
