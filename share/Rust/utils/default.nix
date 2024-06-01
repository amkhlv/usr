{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { } }:

rustPlatform.buildRustPackage rec {
  name = "amkhlv-utils";
  version = "1.0";
  src = builtins.path { path = ./.; name = "amkhlv-utils"; };

  cargoHash = "sha256-fDrXBJDS8EmieH/V754E8//vmjmJpFXTbyIQqu/XuGw=";
  meta = with stdenv.lib; {
    description = "various utils";
  };
  nativeBuildInputs = [ pkgs.pkg-config  pkgs.gcc ];
  buildInputs = [ pkgs.pkg-config pkgs.glibc pkgs.glib pkgs.openssl ];
}
