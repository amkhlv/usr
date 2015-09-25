#!/bin/bash

# behaviour when window activates:
xfconf-query -c xfwm4 -p "/general/activate_action" -s switch

# disble automount etc:
xfconf-query -c thunar-volman -p "/automount-drives/enabled" -s false
xfconf-query -c thunar-volman -p "/automount-media/enabled"  -s false
xfconf-query -c thunar-volman -p "/autoopen/enabled" -s false
xfconf-query -c thunar-volman -p "/autobrowse/enabled" -s false
xfconf-query -c xfce4-power-manager -p "/xfce4-power-manager/lid-action-on-ac" -s 0
xfconf-query -c xfce4-power-manager -p "/xfce4-power-manager/lid-action-on-battery" -s 0

# hinting:
xfconf-query -c xsettings -p /Xft/HintStyle -s hintfull
xfconf-query -c xsettings -p /Xft/Antialias -s 1

## I do not use any more, because I now use xbindkeys
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Alt>k'  -s 'wmjump -v  --rich --timeout=5 >> /tmp/wmjump-log.txt'
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Alt>j'  -s 'wmjump -v --current --rich --timeout=5 >> /tmp/wmjump-log.txt'
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Super>p'  -s 'gmrun'
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Super>b'  -s "xte 'mousemove 2 70'"
# xfconf-query -c xfce4-keyboard-shortcuts  --create -t "string" -p '/commands/custom/<Super>y'  -s "xte 'usleep 250000' 'mouseclick 2'"
