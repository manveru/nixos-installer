with import <nixpkgs> {};
let
  guile-json = callPackage ./guile-json.nix {};
in
callPackage ./. { inherit guile-json; }
