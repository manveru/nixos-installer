{config, pkgs, ... }:
{
  services = {
    xserver = {
      enable = true;
      autorun = true;
      exportConfiguration = true;
      desktopManager = {
        default = "none";
        xterm.enable = false;
      };
      windowManager = {
        i3.enable = true;
        i3.package = pkgs.i3-gaps;
        default = "i3";
      };
      enableCtrlAltBackspace = true;
    };
  };
}
