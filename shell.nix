with import <nixpkgs> {};
let
  guile-json = callPackage ./guile-json.nix {};
in
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [ guile entr utillinux jq guile-json ];

  TZDIR = "${tzdata}/share/zoneinfo";

  GUILE_LOAD_PATH="${guile-json}";
}
