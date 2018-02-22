
CLI
===

Main commands
-------------

To exit back to menu, press `ESC`

The first thing to do is:

    set pager=1

This causes long output to scroll nicely.

Then execute:

    ls

This will list all devices, like `(hd0,msdos1)`. To list files on specific device:

    ls (hd0,msdos1)/

The NORMAL shortcut
-------------------

    set root=(hd0,msdos1)
    set prefix=(hd0,msdos1)/boot/grub
    insmod normal
    normal

This should bring the menu.

Long way
--------

Then:

    set root=(hd0,msdos1)

To boot:

    linux /boot/vmlinuz-... root=/dev/sda...
    initrd /boot/initrd.img-...
    boot

To find boot partitions/devices:

    search -f /boot/grub/*


Miscellaneous commands
----------------------

To remove the background image: `background_image` (without arguments). This results in black background.


Configuration
=============

Configuration file: `/etc/default/grub`

Background image
----------------

    GRUB_BACKGROUND=/path/to/filename.png


Resolution
----------

First have to figure out available resolutions:

    hwinfo --framebuffer

Then put into `/etc/default/grub`:

    GRUB_GFXMODE=1152x864x24
    GRUB_GFXPAYLOAD_LINUX=keep

Moreover, it seems that I need:

    echo "FRAMEBUFFER=y"   > /etc/initramfs-tools/conf.d/splash
