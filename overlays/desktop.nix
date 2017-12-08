{...}: {
  networking.networkmanager.enable = true;

  environment.systemPackages = [
    coreutils
    ffmpeg
    file
    firefox
    gimp
    gitFull
    glxinfo
    gnome3.dconf
    gnome3.gnome_settings_daemon
    gnome3.networkmanagerapplet
    htop
    inetutils
    inkscape
    libreoffice
    nano
    rsync
    silver-searcher
    unrar
    wget
    which
    xlibs.xev
    xorg.xkill
    xpdf
    zip
  ];
}
