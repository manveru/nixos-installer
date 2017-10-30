with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [ guile entr ];

  TZDIR = "${tzdata}/share/zoneinfo";
}
