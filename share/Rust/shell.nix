with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    cargo rustc glibc glib pkg-config libadwaita gdk-pixbuf pango gtk3 gtk4 openssl cairo pango ncurses gst_all_1.gstreamer gst_all_1.gst-plugins-base gst_all_1.gst-plugins-good gst_all_1.gst-plugins-bad gst_all_1.gst-plugins-ugly gst_all_1.gst-libav gst_all_1.gst-vaapi
  ];
}
