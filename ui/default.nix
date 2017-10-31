{stdenv, lib, elm-make, callPackage}:
let
  makeElmStuff = callPackage ./elm2nix.nix {};
  elmStuff = makeElmStuff (import ./package.nix);
in
stdenv.mkDerivation {
  name = "nixos-intaller-frontend";
  src = lib.cleanSource ./.;
  installPhase = ''
    mkdir -p $out
    mkdir $out/tmp
    cp -R $src/* $out/tmp
    (
      cd $out/tmp
      ls -R
      ${elmStuff}
      ${elm-make}/bin/elm-make --yes --output $out/index.html $src/index.elm
    )
    rm -rf $out/tmp
    cp ${./logo.svg} $out/logo.svg
  '';
}
