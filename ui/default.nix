{stdenv, lib, elm-make}:
stdenv.mkDerivation {
  name = "nixos-intaller-frontend";
  src = lib.cleanSource ./.;
  installPhase = ''
    mkdir -p $out
    cp ${./nixos.svg} $out/nixos.svg
    ${elm-make}/bin/elm-make --yes --output $out/index.html $src/index.elm
  '';
}
