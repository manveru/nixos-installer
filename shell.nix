with import (fetchTarball {
  url = https://github.com/NixOS/nixpkgs/archive/4db6d3589175042c0a4c17691fa5cf855053d7fa.tar.gz;
  sha256 = "0q07hcml3lwk4wgs1jr6i14z05bx32rw12p400nh99zkyjqdws07";
}) {overlays = [ (import ./overlay.nix) ]; };
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [
    elmPackages.elm
    elmPackages.elm-compiler
    elmPackages.elm-make
    elmPackages.elm-package
    elmPackages.elm-reactor
    elmPackages.elm-repl
    entr
    guile
    guile-fibers
    guile-json
    guile-websocket
    jq
    nodejs
    qemu
    utillinux
    yarn
  ];

  TZDIR = "${tzdata}/share/zoneinfo";
  GUILE_LOAD_PATH="${guile-json}";
  NIXOS_MANUAL = nixos-manual;
}
