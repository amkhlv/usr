{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "secrets";
  version = "1.0";
  src = builtins.path { path = ./.; name = "secrets"; };

  cargoHash = "sha256-rpQBAaTniVK4Wm2b4JVzgnuD3Eb8TzM80GQhkTUEoNg=";
  meta = with stdenv.lib; {
    description = "password  manager";
  };
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.pkgconfig ];
}
