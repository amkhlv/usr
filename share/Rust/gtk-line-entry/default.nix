{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "alinen";
  version = "1.0";
  src = builtins.path { path = ./.; name = "alinen"; };

  cargoHash = "sha256-ZMvaBHF53vwDwCBggjmSUnu8tCrPQJ+4eKd+LNzKGdk=";
  meta = with stdenv.lib; {
    description = "line entry dialogue";
  };
  nativeBuildInputs = [ pkgs.pkg-config pkgs.gcc ];
  buildInputs = [ pkgs.pkg-config pkgs.gcc pkgs.cairo pkgs.pango pkgs.gdk-pixbuf pkgs.gtk3 pkgs.glibc pkgs.glib pkgs.xorg.libxcb pkgs.xorg.libXmu pkgs.xorg.xcbutilwm ];
}
