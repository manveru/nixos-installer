{lib, stdenv, elm-make, elmPackageJson, fetchurl}: deps:
let
  json = builtins.toJSON (lib.mapAttrs (name: info: info.version) deps);
  buildPackage = name: info: {
    drv = stdenv.mkDerivation {
      name = lib.replaceChars ["/"] ["-"] name + "-${info.version}";

      src = fetchurl {
        url = "https://github.com/${name}/archive/${info.version}.tar.gz";
        meta.homepage = "https://github.com/${name}/";
        inherit (info) sha256;
      };

      phases = [ "unpackPhase" "installPhase" ];

      installPhase = ''
        mkdir -p $out
        cp -r * $out
      '';
    };
    name = name;
    version = info.version;
  };
  packages = lib.mapAttrsToList buildPackage deps;
  linkedPackages = lib.concatStrings (builtins.map (package: ''
    mkdir -p elm-stuff/packages/${package.name}
    ln -s ${package.drv} elm-stuff/packages/${package.name}/${package.version}
  '') packages);
in
stdenv.mkDerivation {
  name = "elm-stuff";
  unpackPhase = "true";
  installPhase = ''
    set -ex
    mkdir -p $out/elm-stuff
    cd $out
    cat > elm-stuff/exact-dependencies.json <<EOF
    ${json}
    EOF
    ${linkedPackages}
    cp ${elmPackageJson} elm-package.json
    ${elm-make}/bin/elm-make
  '';
}
