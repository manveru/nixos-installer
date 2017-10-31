{ stdenv, fetchgit, guile, autoreconfHook, pkgconfig, texinfo }:
let
  version = "1.0.0";
in
stdenv.mkDerivation {
  name = "guile-fibers-${version}";
  src = fetchgit {
    url = "https://github.com/wingo/fibers.git";
    rev = "b86405a2f8daaee3c5d2fcd1b6cefd8e9543c703";
    sha256 = "0rxy85r0zwyzzzkadls0bcxrmz8969q50w11v4h75phbrn4mdkdc";
  };

  nativeBuildInputs = [ autoreconfHook pkgconfig texinfo ];

  buildInputs = [ guile ];
}
