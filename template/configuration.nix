{config, pkgs, lib, ... }:
with builtins;
let
  json = fromJSON (readFile ./installer.json);
in
json // {
  imports = [
    ./hardware-configuration.nix
    <nixpkgs/nixos/modules/testing/test-instrumentation.nix>
    ./ssh.nix
  ];

  system.extraDependencies = with pkgs; [ stdenvNoCC ];
  boot.loader.grub = {
    version = 2;
    device = "/dev/vda";
    extraConfig = "serial; terminal_output.serial";
    fsIdentifier = "uuid";
  };

  hardware.enableAllFirmware = lib.mkForce false;
  nix.binaryCaches = lib.mkForce [ ];
}
