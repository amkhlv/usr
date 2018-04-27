Debian as an iSCSI Initiator
============================

Instructions are on [https://wiki.debian.org/SAN/iSCSI/open-iscsi](https://wiki.debian.org/SAN/iSCSI/open-iscsi)

Installation
============

    aptitude install open-iscsi

( __rebuilds initram__ ! )

Use
===

Configure initiator name
------------------------

Edit file `/etc/iscsi/initiatorname.iscsi` and set `InitiatorName` to that `iqn.....` thing which is on the server

Discovery
---------

    iscsiadm  -m discovery -t st -p 192.168.0.20

results in something like:

    192.168.0.1:3260,1 iqn.2007-01.org.debian.foobar:example
    192.168.0.1:3260,1 iqn.2007-01.org.debian.foobar:USB

Then:

    iscsiadm  -m node  --targetname "iqn.2007-01.org.debian.foobar:example" --portal "192.168.0.1:3260" --login

    systemctl restart iscsid

This should appear as `/dev/sdx`
