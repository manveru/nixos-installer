self: super:
let
  makeElmStuff = super.callPackage ./ui/elm2nix.nix {
    elm-make = self.elmPackages.elm-make;
    elmPackageJson = ./ui/elm-package.json;
  };
in {
  elmStuff = makeElmStuff (import ./ui/package.nix);
  nixos-manual = super.callPackage ./manual.nix {};
}
