{lib, stdenv, fetchurl}:
deps:
  let json = builtins.toJSON (lib.mapAttrs (name: info: info.version) deps);
      cmds = lib.mapAttrsToList (name: info: let
               pkg = stdenv.mkDerivation {

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
             in ''
               mkdir -p elm-stuff/packages/${name}
               ln -s ${pkg} elm-stuff/packages/${name}/${info.version}
             '') deps;
  in ''
    mkdir elm-stuff
    cat > elm-stuff/exact-dependencies.json <<EOF
    ${json}
    EOF
  '' + lib.concatStrings cmds
