{config, pkgs, ... }:
{
  services.openssh.enable = true;
  networking.firewall = {
    enable = true;
    allowPing = true;
    allowedTCPPorts = [ 22 8080 8081 ];
  };
}
