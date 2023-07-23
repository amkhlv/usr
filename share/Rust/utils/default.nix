{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "amkhlv-utils";
  version = "1.0";
  src = builtins.path { path = ./.; name = "amkhlv-utils"; };

  cargoHash = "sha256-IqKuTzN2uT5e6xfYZ77OARx6IIObrVKz0h66uYNaXNg=";
  meta = with stdenv.lib; {
    description = "various utils";
  };
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.pkgconfig pkgs.glibc pkgs.glib pkgs.openssl ];
}
