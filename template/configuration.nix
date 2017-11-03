{config, pkgs, ... }:
with builtins;
let
  json = fromJSON (readFile ./out.json);
in
json // {
  imports = [
    ./ssh.nix
  ];
}
