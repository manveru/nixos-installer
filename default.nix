{guile, stdenv}: stdenv.mkDerivation {
  name = "nixos-installer";

  buildInputs = [ guile ];

  src = null;

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out
    cp ${./template.scm} $out/template.scm
    cp ${./server.scm} $out/server.scm
    ls -R $out
  '';
}
