#!/bin/bash

# behaviour when window activates:
xfconf-query -c xfwm4 -p "/general/activate_action" -s switch

## I do not use any more, because I now use xbindkeys
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Alt>k'  -s 'wmjump -v  --rich --timeout=5 >> /tmp/wmjump-log.txt'
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Alt>j'  -s 'wmjump -v --current --rich --timeout=5 >> /tmp/wmjump-log.txt'
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Super>p'  -s 'gmrun'
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Super>b'  -s "xte 'mousemove 2 70'"
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Super>y'  -s "xte 'usleep 250000' 'mouseclick 2'"
