Installation
============

    aptitude install libnotify-bin

Starting and configuring the service
====================================

    systemctl --user start xfce4-notifyd

If it does not start
--------------------

    dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY

Configuration
-------------

    xfce4-notifyd-config

Sending short notification
==========================

    notify-send -t 100 "finished copying"


