with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    glibc glib pkg-config gdk-pixbuf pango gtk4
  ];
}
