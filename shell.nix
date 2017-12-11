with import (fetchTarball {
  url = https://github.com/NixOS/nixpkgs/archive/a9b3355d669ab15c947e71429855574cf538d975.tar.gz;
  sha256 = "05wmq7i4ln76j0mm52w2k3wa5cysvgkcacpcd2bvj3afdk38k4sq";
}) {overlays = [ (import ./overlay.nix) ]; };
with builtins;
let
  python = import ./requirements.nix { inherit pkgs; };
in
stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = with elmPackages; [
    entr
    qemu
    utillinux
    python36Packages.pytestrunner
    python36Packages.flake8
    pypi2nix
  ];

  NO_TESTS_OVER_WIRE = "1";

  NIXOS_MANUAL = nixos-manual;
}
