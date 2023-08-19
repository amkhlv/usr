{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "a";
  version = "1.0";
  src = builtins.path { path = ./.; name = "a"; };

  cargoHash = "sha256-E+8LqGXVMcEYyRYhkMGQe+NVkKedqol9N8QH1/JsTxY=";
  meta = with stdenv.lib; {
    description = "text-based menu";
  };
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.pkgconfig pkgs.ncurses ];
}
