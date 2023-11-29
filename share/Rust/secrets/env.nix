{ pkgs ? import <nixpkgs> { } }:

pkgs.rustPlatform.buildRustPackage rec {
  name = "secrets";
  version = "1.0";
  src = builtins.path { path = ./.; name = "secrets"; };

  cargoHash = "sha256-6IWeavtc+XgkPh1/5+dgVzNIk/Y8YxAi1BrOpIDWlIQ=";
  meta = with pkgs.stdenv.lib; {
    description = "password  manager";
  };
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.pkgconfig ];
}
