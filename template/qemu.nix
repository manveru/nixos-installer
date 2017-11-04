{ config, lib, pkgs, ... }:
{ imports =
  [ ./hardware-configuration.nix
    <nixpkgs/nixos/modules/testing/test-instrumentation.nix>
  ];

  system.extraDependencies = with pkgs; [ stdenvNoCC ];
  boot.loader.grub = {
    version = 2;
    device = "/dev/vda";
    extraConfig = "serial; terminal_output.serial";
    fsIdentifier = "uuid";
  };

  boot.loader.grub.configurationLimit = 100;
  users.extraUsers.alice = {
    isNormalUser = true;
    home = "/home/alice";
    description = "Alice Foobar";
  };

  hardware.enableAllFirmware = lib.mkForce false;
}
