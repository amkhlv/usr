# .desktop files

The `.desktop` files should be put in `~/.local/share/applications/`

The autostarted ones should be put in `~/.config/autostart/`

## GUI program for creation of desktop files

    exo-desktop-item-edit --create-new ~/.local/share/applications


Specifications and manuals
==========================

[Specifications](https://specifications.freedesktop.org/desktop-entry-spec/latest/)

[Arch](https://wiki.archlinux.org/index.php/Desktop_entries)

Minimal example
===============

It is best to create __using GUI__ : `exo-desktop-item-edit --create-new ~/.local/share/applications`

Example for `pdq`

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
