{ stdenv, fetchgit, autoconf, automake, guile }:
let
  version = "0.6";
in
stdenv.mkDerivation {
  name = "guile-json-${version}";
  src = fetchgit {
    url = "https://github.com/aconchillo/guile-json.git";
    rev = "a1c323d77ee60f9008e654b02cfe1ed3232d90e1";
    sha256 = "0asm30l85qmryvnpq9ipncyy9nlmrdc84wbbijjma6d4z1jckfdb";
  };

  nativeBuildInputs = [ autoconf automake ];

  buildInputs = [ guile ];

  configurePhase = ''
    autoreconf -vif
    ./configure --prefix=$out
  '';
}
