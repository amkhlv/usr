{ rustPlatform, stdenv, lib, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "amkhlv-utils";
  version = "1.0";
  src = builtins.path { path = ./.; name = "amkhlv-utils"; };

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
    "steel-core-0.6.0" = "sha256-MnuwNRQbXIA05lwCn1saSWpV79wMatleZ5etWZHAODk=";
       };
  };

  #cargoDeps = rustPlatform.importCargoLock {
    #lockFile = ./Cargo.lock;
  #};
  #cargoHash = "";
  meta = with stdenv.lib; {
    description = "various utils";
  };
  nativeBuildInputs = [ pkgs.pkg-config  pkgs.gcc ];
  buildInputs = [ pkgs.pkg-config pkgs.glibc pkgs.glib pkgs.openssl ];
}
