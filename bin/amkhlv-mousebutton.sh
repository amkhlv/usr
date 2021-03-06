#!/bin/bash

# See file:///home/andrei/a/html-doc/keyboard/mapping-mouse-buttons.html

WMC="$(xprop -id `xprop -root _NET_ACTIVE_WINDOW | sed -e's/.*\(0x.*\),.*/\1/g'` | awk '/WM_CLASS/{print $4}')"

date >> /tmp/amkhlv-mousebutton.log
echo "$WMC" >> /tmp/amkhlv-mousebutton.log

[ "$1" ] || { echo $WMC ; exit 0 ; }

case $WMC in
    '"Inkscape"')
        xte 'keydown Control_L' 'key S' 'keyup Control_L'
        ;;
    *)
        true
        ;;
esac
