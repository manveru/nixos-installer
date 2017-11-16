module Overlays exposing (..)


server =
    """
{...}: {
  services = {
    openssh = {
      enable = true;
    };
  };
}
"""
