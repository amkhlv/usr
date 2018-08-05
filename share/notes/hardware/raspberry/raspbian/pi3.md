Preparing for first boot
========================

Enable ssh
----------

Create empty file `ssh` on the boot partition

Old interface naming scheme
---------------------------

Need to include `net.ifnames=0` into the file `cmdline.txt` (which is on the boot partition)

Static IP
---------

__Do not__ modify `/etc/network/interfaces`

Instead, `/etc/dhcpcd.conf` should contain lines:

    interface eth0
    static ip_address=192.168.12.1/24

First login
===========

Login: `pi`

Password: `raspberry`

Enable SSH permamently
======================

    systemctl enable ssh

Enable root user
================

    sudo passwd

