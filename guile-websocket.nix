{ stdenv, autoreconfHook, guile, fetchgit, guile-fibers }:
let
  version = "0.1";
in
stdenv.mkDerivation {
  name = "guile-websocket-${version}";
  src = fetchgit {
    url = "https://github.com/a-guile-mind/guile-websocket.git";
    rev = "30dd173b7bd0e8b0053be92ccea73745bbaced15";
    sha256 = "1pn1wrcshf05d6viz72q7m6zdndjrrnzmwl09prihlphrdgs92f3";
  };

  nativeBuildInputs = [ autoreconfHook ];
  buildInputs = [ guile guile-fibers ];
}
