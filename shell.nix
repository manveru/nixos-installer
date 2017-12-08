with import (fetchTarball {
  url = https://github.com/NixOS/nixpkgs/archive/a9b3355d669ab15c947e71429855574cf538d975.tar.gz;
  sha256 = "05wmq7i4ln76j0mm52w2k3wa5cysvgkcacpcd2bvj3afdk38k4sq";
}) {overlays = [ (import ./overlay.nix) ]; };
with builtins;
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = with elmPackages; [
    elm
    elm-compiler
    elm-make
    elm-package
    elm-reactor
    elm-repl
    elm-interface-to-json
    entr
    jq
    nodejs
    qemu
    utillinux
    yarn
    go
  ];

  shellHook = ''
    export PATH="$PATH:${toString ./node_modules/.bin }"
  '';

  TZDIR = "${tzdata}/share/zoneinfo";
  NIXOS_MANUAL = nixos-manual;
}
