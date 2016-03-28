
Rescanning
==========

First I have to make sure that the disk actually rotates (enough power). If it does, I have to figure out
on which `scsi_host` it is:

    cat /sys/class/scsi_host/host0/proc_name
    cat /sys/class/scsi_host/host1/proc_name
    ...

The result is typically either something like  `ata_piix` (or similar, depending on chipset) or `usb-storage`

Obviously, we need `ata`; then rescan- - -:

    echo "- - -" > /sys/class/scsi_host/host1/scan

(three dashes). I think this can be done just on every `hostN`; nothing bad will happen if the disk is already present there.

This should activate all `SATA` disks.

Desactivating
=============

    echo 1 > /sys/block/sdb/device/delete

(this will also park, presumably; at least I can hear the clicking sound)


Terminology
===========

`LUN` = "logical unit number"

The client is called "SCSI initiator" and the server is called a "SCSI target".
Typically, a computer is an initiator and a data storage device is a target. 

To see which disk is which target:

    ls -lh /sys/block/sd*

This should give something like:

    lrwxrwxrwx 1 root root 0 Mar 26 00:03 /sys/block/sda -> ../devices/pci0000:00/0000:00:1f.2/ata2/host1/target1:0:1/1:0:1:0/block/sda
    lrwxrwxrwx 1 root root 0 Mar 26 00:31 /sys/block/sdb -> ../devices/pci0000:00/0000:00:1f.2/ata2/host1/target1:0:0/1:0:0:0/block/sdb

