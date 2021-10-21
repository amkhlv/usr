
USB passthrough
===============

We determine `hostbus` and `hostport` by running:

    lsusb -t

Adding on running virtual machine 
---------------------------------

via [QEMU Monitor](https://en.wikibooks.org/wiki/QEMU/Monitor):

    device_add qemu-xhci,id=usb,bus=pci.0,addr=0x4 
    device_add usb-host,hostbus=5,hostport=1.3.4,id=someid

(and then `device_del`)

This can be confirmed by:

    info usb

Adding via command line options
-------------------------------

    -device qemu-xhci,id=usb,bus=pci.0,addr=0x4 \
    -device usb-host,hostbus=5,hostport=1.2.3 



