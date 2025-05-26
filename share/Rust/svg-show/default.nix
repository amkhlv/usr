{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { }}:

rustPlatform.buildRustPackage rec {
  name = "svg-show";
  version = "1.0";
  src = builtins.path { path = ./.; name = "things"; };

  cargoHash = "sha256-+MLcF6DuVgcWW2vMXPwmWYA/O1dfny6oQ7tY1TwXJ1o=";
  meta = with stdenv.lib; {
    description = "launcher";
  };
  nativeBuildInputs = [ pkgs.pkg-config pkgs.gcc ];
  buildInputs = [ pkgs.pkg-config pkgs.gcc pkgs.cairo pkgs.pango pkgs.gdk-pixbuf pkgs.gtk4 pkgs.glibc pkgs.glib pkgs.xorg.libxcb pkgs.xorg.libXmu pkgs.xorg.xcbutilwm ];
}
