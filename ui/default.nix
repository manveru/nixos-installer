{stdenv, lib, elm-make, callPackage}:
let
  makeElmStuff = callPackage ./elm2nix.nix {
    inherit elm-make;
    elmPackageJson = ./elm-package.json;
  };
  elmStuff = makeElmStuff (import ./package.nix);
  ignore = map (path: toString path ) [
    ./elm-stuff
    ./Makefile
  ];
in
stdenv.mkDerivation {
  name = "nixos-intaller-frontend";

  src = builtins.filterSource (path: type:
    (lib.all (i: i != path) ignore)
  ) ./.;

  installPhase = ''
    mkdir -p $out/elm-stuff
    cd $out
    ln -s ${elmStuff}/elm-package.json elm-package.json
    ln -s ${elmStuff}/elm-stuff/packages elm-stuff/packages
    ln -s ${elmStuff}/elm-stuff/exact-dependencies.json elm-stuff
    cp $src/*.elm $out
    ${elm-make}/bin/elm-make --yes --output index.js $src/Main.elm
    cp -R $src/assets .
  '';
}
