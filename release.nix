with import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };
callPackage ./. {}
