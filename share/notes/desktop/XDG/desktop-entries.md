Location of .desktop files
==========================

The `.desktop` files should be put in `~/.local/share/applications/`

The autostarted ones should be put in `~/.config/autostart/`

Specifications and manuals
==========================

[Specifications](https://specifications.freedesktop.org/desktop-entry-spec/latest/)

[Arch](https://wiki.archlinux.org/index.php/Desktop_entries)

Minimal example
===============

`pdq.desktop`:

    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=pdq PDF viewer
    Exec="/usr/local/bin/pdq" %f

Validation
----------

    desktop-file-validate pdq.desktop

Autostarting
============

see [my writeup on autostarting](../xfce4/autostart.md)
