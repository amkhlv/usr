{
  lib,
  rustPlatform,
  pkg-config,
  gcc,
  wrapGAppsHook4,
  cairo,
  pango,
  gdk-pixbuf,
  gtk4,
  librsvg,
  glibc,
  glib,
  libxcb,
  libxmu,
  libxcb-wm,
}:

rustPlatform.buildRustPackage rec {
  pname = "svg-show";
  version = "0.1.0";
  src = builtins.path { path = ./.; name = "svg-show"; };

  cargoHash = "sha256-+MLcF6DuVgcWW2vMXPwmWYA/O1dfny6oQ7tY1TwXJ1o=";

  nativeBuildInputs = [
    pkg-config
    gcc
    wrapGAppsHook4
  ];

  buildInputs = [
    cairo
    pango
    gdk-pixbuf
    gtk4
    librsvg
    glibc
    glib
    libxcb
    libxmu
    libxcb-wm
  ];

  meta = with lib; {
    description = "launcher";
    mainProgram = "svg-show";
  };
}
