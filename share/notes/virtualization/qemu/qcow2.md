Create qcow2 image
==================

    qemu-img create -f qcow2 foobar.qcow2 10G

Convert VDI to QCOW2
====================

    qemu-img convert  -f vdi  -O qcow2  from.vdi  to.qcow2



Mount QCOW2
===========

Same as [VDI](../virtualbox/mount-vdi.md)
