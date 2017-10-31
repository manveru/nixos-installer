with import <nixpkgs> {overlays = [ (import ./overlay.nix) ]; };
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [
    guile
    entr
    utillinux
    jq
    guile-json
    guile-websocket
    guile-fibers
    elmPackages.elm
    elmPackages.elm-compiler
    elmPackages.elm-make
    elmPackages.elm-package
    elmPackages.elm-reactor
    elmPackages.elm-repl
  ];

  TZDIR = "${tzdata}/share/zoneinfo";

  GUILE_LOAD_PATH="${guile-json}";

  shellHook = ''
    rm -rf elm-stuff
    ${elmStuff}
  '';
}
