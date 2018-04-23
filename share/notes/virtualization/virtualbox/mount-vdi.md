Using qemu
==========

As [explained in WikiBook on Qemu](https://en.wikibooks.org/wiki/QEMU/Images#Mounting_an_image_on_the_host) :

Mount VDI
---------

    rmmod nbd
    modprobe nbd max_part=16
    qemu-nbd -c /dev/nbd0 drive.vdi
    partx -a /dev/nbd0

    mount /dev/ndb0p1 /my/mountpoint

Unmount VDI
-----------

    qemu-nbd -d /dev/nbd0

