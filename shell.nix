with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [ guile entr utillinux jq ];

  TZDIR = "${tzdata}/share/zoneinfo";
}
