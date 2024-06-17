
let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  pkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustChannel = pkgs.rustChannelOf {
     channel = "stable";
  };
  rust = (rustChannel.rust.override {
    targets = [
      "x86_64-unknown-linux-musl"
    ];
    extensions = ["rust-src" "rust-analysis"];
  });
in
  with pkgs;
  mkShell {
    nativeBuildInputs = [ pkg-config gcc udev ];
    buildInputs = [
      cargo-zigbuild
      rust
      cmake 
      glib 
      pkg-config 
      libadwaita 
      xorg.libxcb 
      xorg.libXmu 
      xorg.xcbutilwm 
      gdk-pixbuf 
      pango 
      gtk3 
      gtk4 
      openssl 
      cairo 
      pango 
      ncurses 
      udev 
    ];  
  }
