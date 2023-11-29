with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    glibc glib pkg-config gdk-pixbuf pango gtk3 gtk4 openssl cairo pango ncurses
  ];
}
