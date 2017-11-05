{stdenv, lib, elm-make, callPackage}:
let
  makeElmStuff = callPackage ./elm2nix.nix {};
  elmStuff = makeElmStuff (import ./package.nix);
  ignore = map (path: toString path ) [
    ./elm-stuff
  ];
in
stdenv.mkDerivation {
  name = "nixos-intaller-frontend";

  src = builtins.filterSource (path: type:
    (lib.all (i: i != path) ignore)
  ) ./.;

  installPhase = ''
    mkdir -p $out
    mkdir $out/tmp
    cp -R $src/* $out/tmp
    (
      cd $out/tmp
      ${elmStuff}
      ${elm-make}/bin/elm-make --yes --output $out/index.js $src/Main.elm
    )
    rm -rf $out/tmp
    cp ${./logo.svg} $out/logo.svg
  '';
}
