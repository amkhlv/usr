
Application autostart configuration directory
=============================================

Autostarted applications are listed as `.desktop` files in `~/.config/autostart/`


Format of .desktop files
========================

Specifications and manuals
--------------------------

[Specifications](https://specifications.freedesktop.org/desktop-entry-spec/latest/)

[Arch](https://wiki.archlinux.org/index.php/Desktop_entries)


Validation
----------

    desktop-file-validate  amkhlv-xfce4-start.desktop

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

