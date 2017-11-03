{ system ? builtins.currentSystem }:
with import <nixpkgs/nixos/lib/testing.nix> { inherit system; };
with import <nixpkgs/nixos/lib/qemu-flags.nix>;
let
  pkgs = import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };
  installer = pkgs.callPackage ./. {};
in
with pkgs.lib;
makeTest {
  name = "nixos-installer-test";

  nodes = {
    box = {config, pkgs, ...}: {
      time.timeZone = "Europe/Berlin";
      i18n.defaultLocale = "en_US.UTF-8";

      environment.systemPackages = with pkgs; [
        htop iotop tmux strace file gzip
      ];

      systemd.services."installer-service" = {
        wantedBy = [ "multi-user.target" ];
        script = "${installer}/bin/start-installer";
        serviceConfig.WorkingDirectory = installer;
      };

      nix = {
        package = pkgs.nixStable;
        nrBuildUsers = 10;
        useSandbox = true;
        readOnlyStore = true;
      };

      virtualisation.diskSize = 2048;
    };
  };

  testScript = ''
    startAll;

    $box->waitForUnit("installer-service.service");
    $box->waitForOpenPort(8081);

    subtest "Shows up", sub {
      $box->succeed("curl -f http://localhost:8081");
      $box->succeed("nixos-install");
    };

    $box->stopJob("installer-service");
    $box->shutdown
  '';
}
