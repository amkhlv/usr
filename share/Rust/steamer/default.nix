{ rustPlatform, stdenv, pkgs ? import <nixpkgs> { }}:

rustPlatform.buildRustPackage rec {
  name = "amkhlv-steamer";
  version = "1.0";
  src = builtins.path { path = ./.; name = "amkhlv-steamer"; };

  cargoHash = "sha256-zDAJQMINvB/FYvHmwp5Q5f/jLQl4op2xTs2X9xWkKtU=";
  meta = with stdenv.lib; {
    description = "steamer";
  };
  nativeBuildInputs = [ pkgs.pkg-config pkgs.gcc pkgs.udev ];
  buildInputs = [ pkgs.pkg-config pkgs.udev pkgs.gcc pkgs.cairo pkgs.pango pkgs.gdk-pixbuf pkgs.gtk4 pkgs.glibc pkgs.glib pkgs.xorg.libxcb pkgs.xorg.libXmu pkgs.xorg.xcbutilwm ];
}
