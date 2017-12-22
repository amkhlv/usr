We follow [instructions here](https://wiki.openwrt.org/doc/howto/extroot) :

Needed packages
===============

    PACKAGES="blkid block-mount kmod-usb2 kmod-usb-storage kmod-fs-ext4 kmod-scsi-core"

Copying files to overlay
========================

    mount /dev/sda1 /mnt ; tar -C /overlay -cvf - . | tar -C /mnt -xf - ; umount /mnt

Mounting overlay
================

Instead of the usual `/etc/fstab`, `OpenWRT` uses a configuration file `/etc/config/fstab`, which
does not use the standard `fstab` syntax, and uses instead the `OpenWRT` config syntax.

[See the manual](https://wiki.openwrt.org/doc/uci/fstab)

A tentative `fstab` file is created as follows:

    block detect > /etc/config/fstab

Mounting at boot time should be enabled:

    /etc/init.d/fstab enable

The parameter `root_delay` in `/etc/config/fstab` should usually be increased to something like 15 seconds.
