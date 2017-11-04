Minimal install of XFCE4
========================

    aptitude install  xfwm4  xfwm4-themes  xfce4-settings  xfce4-panel

Configuration
=============

Settings Manager
----------------

__Some settings__ (_e.g._ Window Manager Tweaks) can be done by:

    xfce4-settings-manager

which has a nice GUI. Things like `XFWM4` keyboard shortcuts are configured the `Window Manager` section.

Direct edit of XML files
------------------------

They are in `~/.config/xfce4/`; I understand that the command:

    xfce4-settings-editor

is just an XML editor for them.

Autostart
---------

See [autostart.md](autostart.html)


