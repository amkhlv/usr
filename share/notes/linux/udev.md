On adding device
================

The line in `/etc/udev/rules.d/10-local.rules`:

    KERNEL=="sd[a-z]?", ATTRS{serial}=="AAAA123123123", ACTION=="add", RUN+="/bin/true"

Lock screen on pendrive removal
===============================

The line in `/etc/udev/rules.d/10-local.rules`:

    ENV{ID_SERIAL_SHORT}=="AAA123123123", ENV{DEVTYPE}=="usb_device", ACTION=="remove", RUN+="/bin/su andrei -c 'export DISPLAY=:0 && /usr/bin/gnome-screensaver-command -l'"
