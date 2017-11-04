Xdefaults is now Xresources
===========================

The Gnome loads `.Xresources`, not `.Xdefaults`. Symlink.

GSettings and DConf
===================

Get list of settings
--------------------

    dconf dump / 

lists some subset of settings, but definitely not all.

Map MENU or Caps_Lock to overlay key
------------------------------------

    dconf write /org/gnome/mutter/overlay-key "'Menu'"

If the `Menu` key is not available on the keyboard, we can map it to `Caps_Lock`:

    setxkbmap -option caps:menu

See [writeup on setxkbmap](../../linux/keyboard.md)

Keyboard shortcuts to custom commands
-------------------------------------

    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/name "'altgr-intl'" 
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/binding "'<Super>F8'" 
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/command "'/usr/bin/setxkbmap -layout us -variant altgr-intl -option nodeadkeys'"

    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/name "'ru-phonetic'" 
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/binding "'<Super>F7'" 
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/command "'/usr/bin/setxkbmap ru phonetic'"

    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/name "'pt-br'" 
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/binding "'<Super>F6'"
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/command "'/usr/bin/setxkbmap -model abnt2 -layout br -variant abnt2'"

