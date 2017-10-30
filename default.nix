{stdenv, lib, guile, jq}: stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [ guile jq ];

  src = lib.cleanSource ./.;

  doInstallCheck = true;

  installPhase = ''
    mkdir -p $out #test
    cp *.scm tests/ lib/ $out -R
  '';

  installCheckPhase = ''
    cd $out
    set +e
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
