{stdenv, lib, elm-make, callPackage}:
let
  makeElmStuff = callPackage ./elm2nix.nix {
    inherit elm-make;
    elmPackageJson = ./elm-package.json;
  };
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
    mkdir -p $out/elm-stuff
    ln -s ${elmStuff}/elm-stuff/packages $out/elm-stuff/packages
    ln -s ${elmStuff}/elm-stuff/exact-dependencies.json $out/elm-stuff
    ${elm-make}/bin/elm-make --yes --output $out/index.js $src/Main.elm
    cp -R $src/assets $out
    ls -R $out
  '';
}
