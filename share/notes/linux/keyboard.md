How to Find keycode or keysym of a Key
======================================

    xev

setxkbmap
=========

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

