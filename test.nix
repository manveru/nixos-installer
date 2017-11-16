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
      imports = [
        ./template/nixos-installer-service.nix
      ];

      time.timeZone = "Europe/Berlin";
      i18n.defaultLocale = "en_US.UTF-8";

      environment.systemPackages = with pkgs; [
        htop iotop tmux strace file gzip
      ];

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

    $box->waitForUnit("nixos-installer");
    $box->waitForOpenPort(8081);

    subtest "Shows up", sub {
      $box->sleep(2);
      $box->execute("curl -v http://localhost:8081 >&2");
      $box->succeed("systemctl status nixos-installer >&2");
      $box->succeed("journalctl -xe -u nixos-installer >&2");
      $box->succeed("curl -vf http://localhost:8081 >&2");
    };

    $box->stopJob("nixos-installer");
    $box->shutdown
  '';
}
