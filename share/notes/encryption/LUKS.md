luksFormat
==========

    cryptsetup --key-size 512 --hash sha256 --iter-time 2000 luksFormat ...

Detached header
===============

    cryptsetup --key-size 512 --header /dev/sdd1 --align-payload=0 luksFormat /dev/sdb


Backup headers in a file
========================

    cryptsetup --header /dev/sdXn --header-backup-file header_MyDisk.img luksHeaderBackup /dev/sdYm

Restore headers from a file
===========================

    cryptsetup luksHeaderRestore /dev/sdXn --header-backup-file header_MyDisk.img

