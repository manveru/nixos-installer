{...}: {
  nix.maxJobs = 8;
  services = {
    xserver = {
      enable = true;
    };
  };
}
