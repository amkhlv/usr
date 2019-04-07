Auto login to console on boot
=============================

Copy file `/usr/lib/systemd/system/getty@.service` to `/etc/systemd/system/getty@tty1.service` and replace the line:

    ExecStart=-/sbin/agetty -o '-p -- \\u' --noclear %I $TERM

with:

    ExecStart=-/sbin/agetty -a root --noclear %I $TERM

Then:

    systemctl enable getty@tty1.service

