{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { }}:

rustPlatform.buildRustPackage rec {
  name = "things";
  version = "1.0";
  src = builtins.path { path = ./.; name = "things"; };

  cargoHash = "sha256-SqTXGvKxD8RSednN8RAoCpbc0JBkPh7J0lYp2A4iBcU=";
  meta = with stdenv.lib; {
    description = "launcher";
  };
  nativeBuildInputs = [ pkgs.pkg-config pkgs.python3 pkgs.gcc ];
  buildInputs = [ pkgs.pkgconfig pkgs.cairo pkgs.pango pkgs.gdk-pixbuf pkgs.gtk3 pkgs.glibc pkgs.glib  ];
}
