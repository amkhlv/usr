XKB
===

See [the writeup by Иван Паскаль](http://pascal.tsu.ru/other/xkb/setup.html).

How to Find keycode or keysym of a Key
======================================

    xev

setxkbmap
=========

It can "dry-run" by passing the `-print` option:

    setxkbmap   -layout us   -variant altgr-intl   -option nodeadkeys   -print

outputs:

    xkb_keymap {
            xkb_keycodes  { include "evdev+aliases(qwerty)" };
            xkb_types     { include "complete"      };
            xkb_compat    { include "complete"      };
            xkb_symbols   { include "pc+us(altgr-intl)+inet(evdev)+capslock(none)+ctrl(nocaps)"     };
            xkb_geometry  { include "pc(pc104)"     };
    };

This means that the following files are loaded, all rel.to `/usr/share/X11/xkb/` :

1. `keycodes/evdev` and the part `xkb_keycodes "querty" {...}` of `keycodes/aliases`

2. `types/complete`

3. _etc._

To view the current config
--------------------------

    setxkbmap -query

To reset everything
-------------------

    setxkbmap -option

To list all possible options
----------------------------

    cat /usr/share/X11/xkb/rules/base.lst



Generate PDF of keyboard layout
===============================

    setxkbmap -layout us -variant altgr-intl -option nodeadkeys -print | xkbcomp - - | xkbprint -ll 3 - - | ps2pdf - > us-intl.pdf

Notice that `-ll 3` means `AltGr` pressed

Chattering (repeating keys, дробление) keyboard
===============================================

    ~/.config/xfce4/xfconf/xfce-perchannel-xml/accessibility.xml 

should contain:


    <?xml version="1.0" encoding="UTF-8"?>

    <channel name="accessibility" version="1.0">
      <property name="BounceKeys" type="bool" value="true">
        <property name="Delay" type="int" value="30"/>
      </property>
    </channel>

In terms of `xfconf` :

    xfconf-query -c accessibility -p /BounceKeys -s true
    xfconf-query -c accessibility -p /BounceKeys/Delay -s 30

Configuring the Compose key in any layout
=========================================

Choosing the compose key
------------------------

`setxkbmap  -layout us  -variant altgr-intl  -option nodeadkey  -option compose:paus`

X client mappings for multi-key input sequences
-----------------------------------------------

`cp /usr/share/X11/locale/en_US.UTF-8/Compose ~/.XCompose`
