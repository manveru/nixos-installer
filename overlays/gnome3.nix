{...}: {
  gnome3 = {
    tracker.enable = false; # I don't use tracker
    gnome-keyring.enable = true;
  };
  services = {
    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome3.enable = true;
    };
  };
}
