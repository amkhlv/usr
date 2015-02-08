How to install systemd
======================

     aptitude install systemd systemd-sysv

How to remove systemd
=====================

First install the SysV init packages

     apt-get install sysvinit-core sysvinit sysvinit-utils

Then reboot your machine and remove all of the systemd packages

     apt-get remove --purge --auto-remove systemd

How to avoid systemd
====================

Prevent apt from installing systemd packages in the future.

     echo -e 'Package: systemd\nPin: origin ""\nPin-Priority: -1' > /etc/apt/preferences.d/systemd

