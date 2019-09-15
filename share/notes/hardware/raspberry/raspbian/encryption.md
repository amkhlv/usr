Sources
=======

Follow writeup [here](https://carlo-hamalainen.net/2017/03/12/raspbian-with-full-disk-encryption/)

Initram
=======

Prepare chroot
--------------

Need to install:

    aptitude install qemu-user-static

(This  will install `binfmt-support`)

Then copy:

    cp /usr/bin/qemu-arm-static   MY_CHROOT/usr/bin/

Then:

Need crypttab entry
-------------------

__I assume that the name of the root device is /dev/mapper/rt__

For `intram` to be generated correctly, I need an entry in `/etc/crypttab` for the root:

    rt /dev/disk/...  /dev/disk/...:/path/to/my.key  noauto,luks,keyscript=/lib/cryptsetup/scripts/passdev,initramfs

This is __only__ needed for the `initram` generation, and in particular for `passdev` to end up into `initram`.
Notice: 

1. the last `cryptsetup` option: __initramfs__ --- this is what does the magic

2. notice how the key file parameter is of the form `/dev/disk/...:/path/to/my.key` --- the `:` separates device and path on its filesystem; 
   this is specific to `/lib/cryptsetup/scripts/passdev`

systemd bug
-----------

There is a `systemd` bug : [why-does-systemd-cryptsetup-try-to-remount-the-root-partition-already-mounted](https://unix.stackexchange.com/questions/275734/why-does-systemd-cryptsetup-try-to-remount-the-root-partition-already-mounted)
To mitigate this bug, need to mask the `systemd` service:

    systemctl mask systemd-cryptsetup@rt.service

Chroot
------

    systemd-nspawn -D MY_CHROOT

In chroot:

    apt-get update && apt-get full-upgrade
    
    apt-get install busybox cryptsetup

    echo "export CRYPTSETUP=y" >> /etc/initramfs-tools/hooks/forcecryptsetup

    mkinitramfs -o /boot/initramfs.gz 4.4.50-v7+

where `4.4.50-v7+` is taken from `ls /lib/modules`

Need to tell Raspbian to use initram
====================================

    echo "initramfs initramfs.gz followkernel" >> /boot/config.txt

Kernel boot parameters
======================

`/boot/cmdline.txt` should be something like:

    dwc_otg.lpm_enable=0 console=serial0,115200 console=tty1 root=/dev/mapper/rt cryptopts=luks,target=rt,source=/dev/disk/by-partuuid/nnnnnnnnnn,header=/dev/disk/by-partuuid/96333efc-02,keyscript=/lib/cryptsetup/scripts/passdev,key=/dev/by-partuuid/nnnnnnnnnnnnnnn:/somdir/root.key rootfstype=ext4 elevator=deadline fsck.repair=yes rootwait 

Here  __I assume that the name of the root device is /dev/mapper/rt__








    
