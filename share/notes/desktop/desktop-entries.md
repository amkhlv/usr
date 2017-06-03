Location of .desktop files
==========================

The `.desktop` files should be put in `~/.local/share/applications/`

The autostarted ones should be put in `~/.config/autostart/`

Minimal example
===============

    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=pdq PDF viewer
    Exec="/usr/local/bin/pdq" %f
