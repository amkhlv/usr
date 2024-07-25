{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "amkhlv-utils";
  version = "1.0";
  src = builtins.path { path = ./.; name = "amkhlv-utils"; };

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
         "lasso-0.7.2" = "sha256-ccqcDvWZD5hp4iZ420jgNJtR+MaVlqHFtXU2GWkbyfg=";
         "steel-core-0.6.0" = "sha256-35ZepMM0d/EsbiQ1/7x2DicybeXfZI5O37l6Okzc4Xk=";
       };
  };

  cargoDeps = rustPlatform.importCargoLock {
    lockFile = ./Cargo.lock;
  };
  #cargoHash = "";
  meta = with stdenv.lib; {
    description = "various utils";
  };
  nativeBuildInputs = [ pkgs.pkg-config  pkgs.gcc ];
  buildInputs = [ pkgs.pkg-config pkgs.glibc pkgs.glib pkgs.openssl ];
}
