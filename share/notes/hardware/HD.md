Stopping the hard drive for removal
===================================

    hdparm -S 1 /dev/sdX


wait  for 5 seconds for the drive to stop, then remove it; then:


    echo 1 > /sys/block/(whatever)/device/delete


SMART tools
===========

All info about the disk
-----------------------

    smartctl -a /dev/sdb
    smartctl -A /dev/sdb

Long offline test
-----------------

    smartctl -t long /dev/sdb

SCT Error Recovery Control
--------------------------

Excellent writeup [here](https://geektimes.ru/post/92701/) ; to see:

    smartctl -l scterc /dev/sdb

To set 3 sec read wait and 3 sec write wait:

    smartctl -l scterc,30,30 /dev/sdb

