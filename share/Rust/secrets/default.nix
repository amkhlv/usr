{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "secrets";
  version = "1.0";
  src = builtins.path { path = ./.; name = "secrets"; };

  cargoHash = "sha256-7coXOP/GObMLz5ZhEpRIT6tkOqCkTFvLWmLY7YuLV8Q=";
  meta = with stdenv.lib; {
    description = "password  manager";
  };
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.pkgconfig ];
}
