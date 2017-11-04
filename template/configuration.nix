{config, pkgs, lib, ... }:
with builtins;
let
  json = fromJSON (readFile ./installer.json);
in
json // {
  imports = [
    ./hardware-configuration.nix
    <nixpkgs/nixos/modules/testing/test-instrumentation.nix>
  ];

  system.extraDependencies = with pkgs; [ stdenvNoCC ];
  boot.loader.grub = {
    version = 2;
    device = "/dev/vda";
    extraConfig = "serial; terminal_output.serial";
    fsIdentifier = "uuid";
  };

  users.extraUsers.alice = {
    isNormalUser = true;
    home = "/home/alice";
    description = "Alice Foobar";
  };

  hardware.enableAllFirmware = lib.mkForce false;
}
