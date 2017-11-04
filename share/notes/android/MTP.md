Enable MTP
==========

Stands for "Media Transfer Protocol". I think it is __usually enabled by default__. 

In case if it is not, in order to activate it, need to go to developer mode.

Developer mode
--------------

Go to `Settings` ⇨ `About phone` and tap 7 times on `Build number`.

After developer mode is activated, the tab `Developer options` should appear in `Settings`.

Activate MTP
------------

Go to `Developer options` in `Settings` and find `Select USB Configuration` in the `Networking` paragraph.


jmtpfs
======

Installation
------------

    aptitude install jmtpfs

Use
---

    mkdir /mnt/x
    jmtpfs /mnt/x

