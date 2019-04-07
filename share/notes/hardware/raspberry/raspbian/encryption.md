Sources
=======

Follow writeup [here](https://carlo-hamalainen.net/2017/03/12/raspbian-with-full-disk-encryption/)

Notes
=====

Prepare chroot
--------------

Need to install:

    aptitude install qemu-user-static

(This  will install `binfmt-support`)

Then copy:

    cp /usr/bin/qemu-arm-static   MY_CHROOT/usr/bin/

Then:



Chroot
------

    systemd-nspawn -D MY_CHROOT

In chroot:

    apt-get update && apt-get full-upgrade
    
    apt-get install busybox cryptsetup

    echo "export CRYPTSETUP=y" >> /usr/share/initramfs-tools/conf-hooks.d/forcecryptsetup

    mkinitramfs -o /boot/initramfs.gz 4.4.50-v7+

where `4.4.50-v7+` is taken from `ls /lib/modules`

Then need:

    echo "initramfs initramfs.gz followkernel" >> /boot/config.txt

`/boot/cmdline` should be something like:

    dwc_otg.lpm_enable=0 console=serial0,115200 console=tty1 root=/dev/mapper/rt cryptopts=target=rt,source=/dev/disk/by-partuuid/nnnnnnnnnn,header=/dev/disk/by-partuuid/96333efc-02,keyscript=/lib/cryptsetup/scripts/passdev,key=/dev/by-partuuid/nnnnnnnnnnnnnnn:/somdir/root.key rootfstype=ext4 elevator=deadline fsck.repair=yes rootwait 

(Because I keep the key in `/root/keys`  of `initram`)

Storing key separately
======================

Alternatively, I could use the [standard supplied](https://github.com/lhost/pkg-cryptsetup-debian/blob/master/debian/README.initramfs)
`passdev` script:

    cryptopts=...,keyscript=/lib/cryptsetup/scripts/passdev,key=/dev/by-partuuid/nnnnnnnnnnnnnnn:/somdir/root.key





    
