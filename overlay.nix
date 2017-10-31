self: super:
{
  guile-json = super.callPackage ./guile-json.nix {};
  guile-fibers = super.callPackage ./guile-fibers.nix {};
  guile-websocket = super.callPackage ./guile-websocket.nix {};
}
