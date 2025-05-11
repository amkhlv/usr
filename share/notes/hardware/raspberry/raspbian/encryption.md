Cryptsetup-initramfs
====================

To force `cryptsetup` into `initram`:

    aptitude install cryptsetup-initramfs


    echo "export CRYPTSETUP=y" >> /etc/initramfs-tools/hooks/forcecryptsetup

For `intram` to be generated correctly, I need an entry in `/etc/crypttab` for the root:

    rt /dev/sda1  /dev/disk/...:/path/to/my.key  noauto,luks,keyscript=/lib/cryptsetup/scripts/passdev,initramfs

__Important__ : the source device should be listed as `/dev/sdaN` and not `/dev/disk/by-uuid/...`; otherwize the crypto module will not be in initram.

BTRFS
=====

If using `btrfs`, need to:

    echo btrfs >> /etc/initramfs-tools/modules

The __subvol__ for root system can be specified in `cmdline.txt` like this:

    rootflags=subvol=/myroot

Update Initramfs 
================

    update-initramfs -u -kall

This does not yet work
======================

For some reason, need to:

    apt install --reinstall --yes raspberrypi-bootloader raspberrypi-kernel

and then again:

    update-initramfs -u -kall


Kernel boot parameters
======================

`/boot/cmdline.txt` should be something like:

    dwc_otg.lpm_enable=0 console=serial0,115200 console=tty1 root=/dev/mapper/rt cryptopts=luks,target=rt,source=/dev/disk/by-partuuid/nnnnnnnnnn,header=/dev/disk/by-partuuid/96333efc-02,keyscript=/lib/cryptsetup/scripts/passdev,key=/dev/by-partuuid/nnnnnnnnnnnnnnn:/somdir/root.key rootfstype=ext4 elevator=deadline fsck.repair=yes rootwait 

Here  __I assume that the name of the root device is /dev/mapper/rt__


    
