targetcli-fb
============

    aptitude isntall targetcli-fb

See [Arch writeup](https://wiki.archlinux.org/index.php/ISCSI_Target) and more complete [LIO manual](http://www.linux-iscsi.org/wiki/Targetcli)

Basically, just execute: 

    targetcli

it will open a special shell, then type `help`

The standard port is `3260`  (`iscsi-target`)


Typical targetcli setup
=======================

(from [Arch writeup](https://wiki.archlinux.org/index.php/ISCSI_Target) )

    targetcli

    /> cd backstores/block
    /backstores/block> create  mystorage  /dev/disk/by-id/...
    ...> cd /iscsi
    /iscsi> create
    /iscsi> cd iqn......
    
Then:

    /iscsi/iqn.../tpg1> cd luns
    /iscsi/iqn.../tpg1/luns> create /backstores/block/mystorage
    /iscsi/iqn.../tpg1/luns> cd ../portals
    /iscsi/iqn.../tpg1/portals> create
    /iscsi/iqn.../tpg1/portals> cd ../acls
    /iscsi/iqn.../tpg1/acls> create iqn........
    /iscsi/iqn.../tpg1/acls> cd /
    /> saveconfig

