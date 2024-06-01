with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    cargo rustc rust-analyzer fltk cmake glibc glib pkg-config libadwaita gdk-pixbuf pango gtk3 gtk4 openssl cairo pango ncurses 
  ];
}
