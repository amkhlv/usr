{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "amkhlv-utils";
  version = "1.0";
  src = builtins.path { path = ./.; name = "amkhlv-utils"; };

  cargoHash = "sha256-2cTeLdwu2xHKYegCxFvnPf6R4WSrtEFJxrYIRWT+PZ4=";
  meta = with stdenv.lib; {
    description = "various utils";
  };
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.pkgconfig pkgs.glibc pkgs.glib pkgs.openssl ];
}
