How to disable TrackPoint in Dell
=================================

    sudo aptitude install xinput

Then:

    xinput list

The list will include something like "AlpsPS/2 ALPS DualPoint Stick"

To disable it:

    xinput set-prop "AlpsPS/2 ALPS DualPoint Stick" "Device Enabled" 0

To disable on startup
=====================

See [writeup on autostarting in XFCE4](../desktop/xfce4/autostart.md)



