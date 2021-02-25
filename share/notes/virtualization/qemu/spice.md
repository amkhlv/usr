SPICE
=====

Host
----

    aptitude install remmina remmina-plugin-spice

Guest
-----

`qemu-system-x86_64` should be started with:


    -vga qxl \
    -spice port=....,addr=....,password=....... \
    -device virtio-serial-pci \
    -device virtserialport,chardev=spicechannel0,name=com.redhat.spice.0 -chardev spicevmc,id=spicechannel0,name=vdagent

Inside the guest system, should install:

    aptitude install spice-vdagent

