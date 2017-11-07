self: super:
let
  makeElmStuff = super.callPackage ./ui/elm2nix.nix {
    elm-make = self.elmPackages.elm-make;
    elmPackageJson = ./ui/elm-package.json;
  };
in {
  guile-json = super.callPackage ./guile-json.nix {};
  guile-fibers = super.callPackage ./guile-fibers.nix {};
  guile-websocket = super.callPackage ./guile-websocket.nix {};
  elmStuff = makeElmStuff (import ./ui/package.nix);
  nixos-manual = super.callPackage ./manual.nix {};
}
