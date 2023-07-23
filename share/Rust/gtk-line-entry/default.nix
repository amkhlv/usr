{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "alinen";
  version = "1.0";
  src = builtins.path { path = ./.; name = "alinen"; };

  cargoHash = "sha256-SENd40WW6AMkYW2LpsIT3o4WhKF5iCW80P1hpkmVik0=";
  meta = with stdenv.lib; {
    description = "line entry dialogue";
  };
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.pkgconfig pkgs.cairo pkgs.pango pkgs.gdk-pixbuf pkgs.gtk3 pkgs.glibc pkgs.glib  ];
}
