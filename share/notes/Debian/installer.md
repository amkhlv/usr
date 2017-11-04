Debian Installer
================

This [essentially a flavor of Debian](d-i.alioth.debian.org/doc/internals/index.html) which has to be very small.
So, there are special "lean" packages called `udeb` which are installed by `anna-install`. They are all on the
installer disk, but not in initram.

Using cryptsetup from Debian Installer
======================================

Inside the Debian Installer console:

    anna-install cryptsetup-udeb partman-crypto-dm 
    depmod -a 
    cryptsetup luksOpen /dev/sdc1 asdf 


