{pkgs, ...}:
let
  pkgs = import <nixpkgs> { overlays = [ (import ../overlay.nix) ]; };
  installer = pkgs.callPackage ../. {};
in
{
  systemd.services."nixos-installer" = {
    wantedBy = [ "multi-user.target" ];
    environment = {
      "INSTALLER_SAVE_FILE" = "/mnt/etc/nixos/installer.json";
      "INSTALLER_CONF_FILE" = "/mnt/etc/nixos/configuration.nix";
    };
    serviceConfig =
      { ExecStart = "${installer}/bin/start-installer";
        WorkingDirectory = "${installer}";
        # Restart = "always";
      };
  };
}
