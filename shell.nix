with import <nixpkgs> {};
let
  guile-json = callPackage ./guile-json.nix {};
in
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [
    guile
    entr
    utillinux
    jq
    guile-json 
    elmPackages.elm
    elmPackages.elm-compiler
    elmPackages.elm-make
    elmPackages.elm-package
    elmPackages.elm-reactor
    elmPackages.elm-repl
  ];

  TZDIR = "${tzdata}/share/zoneinfo";

  GUILE_LOAD_PATH="${guile-json}";
}
