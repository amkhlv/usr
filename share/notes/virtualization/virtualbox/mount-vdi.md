Using qemu
==========

Mount VDI
---------

    rmmod nbd
    modprobe nbd max_part=16
    qemu-nbd -c /dev/nbd0 drive.vdi

Unmount VDI
-----------

    qemu-nbd -d /dev/nbd0

