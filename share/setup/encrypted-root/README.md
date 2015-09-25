Preparation
===========

Preparing `inintramfs-tools`
----------------------------

    cp files/sbin/* /usr/local/sbin/
    cp files/gpg_cryptroot /etc/initramfs-tools/hooks/
    cp files/modules /etc/initramfs-tools/
    cp files/conf_cryptroot /etc/initramfs-tools/conf.d/
    gvim /etc/initramfs-tools/conf.d/conf_cryptroot

Note: if the list of modules in `/etc/initramfs-tools/modules` is incomplete, then the boot will fail, most likely `cryptsetup` wil
fail. How to obtain the list of all possible modules. Look at those modules already in `/etc/initramfs-tools/modules`. For example
there is a module called `cbc`. The command `/sbin/modinfo cbc` print information, in particular on where the corresponding `.ko` file
is located. You may go in that folder and see what other `.ko` files are there. (Notice tha the name of the `.ko` file is not
necessarily the name of the module, and certainly the name of the module does not contain the extension `.ko`.)

Preparing `grub`
----------------

    gvim -p files/grub /etc/default/grub

Copying files
-------------

    cp files/sbin/amkhlv-* /usr/local/sbin/


Encrypting the key
==================

Preparing the `GnuPG` on the USB key
------------------------------------

See my writeup in [notes/encryption/OpenPGP.md](../notes/encryption/OpenPGP.md).
I usually name the recipient as follows: `boot-hostname`. Although this should
not matter for anything.

__Always call the encrypted key as follows:__ `hostname.gpg`;

Then the keyscript parameter is just `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` --- the UUID of the key partition; the scripts assume that
the name of the encrypted key is `hostname.gpg`.

If the key partition contains the file `hostname` (without the extension `.gpg`) then the scripts assume that this is the unencrypted
key file.


Adding the key to the encrypted partition
=========================================

    cryptsetup luksAddKey /dev/sda1 /dev/shm/belavista

Finally
=======

1. Dont forget to __remove__ the keyfile from `/dev/shm`.

2. `update-initramfs -kall -u`

3. `grub-update`



