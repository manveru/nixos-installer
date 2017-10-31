{ system ? builtins.currentSystem }:
with import <nixpkgs/nixos/lib/testing.nix> { inherit system; };
let
  pkgs = import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };
in
with pkgs.lib;
let
  guile-json = pkgs.callPackage ./guile-json.nix {};
  installer = pkgs.callPackage ./. {};
in
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
    };

    $box->stopJob("installer-service");
    $box->shutdown
  '';
}
