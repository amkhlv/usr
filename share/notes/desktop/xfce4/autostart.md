
Application autostart configuration directory
=============================================

Autostarted applications are listed as `.desktop` files in `~/.config/autostart/`


Format of .desktop files
========================

See [my writeup](../XDG/desktop-entries.md)

Basic Example
-------------

    [Desktop Entry]

    Version=1.0
    Type=Application
    Name=My Startup
    Exec=/home/andrei/bin/amkhlv-xfce4-start.sh
    Terminal=false
    Hidden=false
    OnlyShowIn=GNOME;XFCE;LXDE;
    X-GNOME-Autostart-enabled=true

Disabling system default items
------------------------------

They are all in `/etc/xdg/autostart/`.

Instead of editing them, override by putting a file with the same name in `~/.config/autostart/`.

For example, to disable `blueman`, put  `blueman.desktop` into `~/.config/autostart/` consisting of just two lines:

    [Desktop Entry]
    Hidden=true

(They will remain on the GUI config menu, but get unchecked)
