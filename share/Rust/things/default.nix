{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { }}:

rustPlatform.buildRustPackage rec {
  name = "things";
  version = "1.0";
  src = builtins.path { path = ./.; name = "things"; };

  cargoHash = "sha256-Uv53DEySoL5vaUBYIinwcR1A3KXHGRGk/fJ1mM9h6yQ=";
  meta = with stdenv.lib; {
    description = "launcher";
  };
  nativeBuildInputs = [ pkgs.pkg-config pkgs.python3 pkgs.gcc ];
  buildInputs = [ pkgs.pkgconfig pkgs.cairo pkgs.pango pkgs.gdk-pixbuf pkgs.gtk3 pkgs.glibc pkgs.glib  ];
}
