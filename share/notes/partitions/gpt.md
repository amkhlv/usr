Transfer gpt partition from one disk to another
===============================================

Suppose that I want to transfer `/dev/sda` to `/dev/sdb`. 
Let us define:

    DISKA=sda
    DISKB=sdb

I first backup the partition table of `disk A`:


    for n in `seq 1 10`; do dd if=/dev/$DISKA$n of=/tmp/$DISKA$n.img ; done
    sgdisk --replicate /dev/$DISKB  /dev/$DISKA

Then __eject /dev/sda__ and say:

    partprobe
    for n in `seq 1 10`; do dd if=/tmp/$DISKA$n.img of=/dev/$DISKB$n ; done

