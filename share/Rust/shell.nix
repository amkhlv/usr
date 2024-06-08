with (import <nixpkgs> {});
mkShell {
  nativeBuildInputs = [ pkg-config gcc udev ];
  buildInputs = [
    cargo rustc rust-analyzer fltk cmake glibc glib pkg-config libadwaita xorg.libxcb xorg.libXmu xorg.xcbutilwm gdk-pixbuf pango gtk3 gtk4 openssl cairo pango ncurses udev 
  ];  
}
