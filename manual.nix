{ ... }:
let
  manual = (import <nixpkgs/nixos/release.nix> {}).manual.x86_64-linux;
in
  "${manual}/share/doc"
