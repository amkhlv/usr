Download and install
====================

From apt
========

    aptitude install hplip

but it is usually an older version, without support for modern printers.

From website
============

Download from [https://developers.hp.com/hp-linux-imaging-and-printing](https://developers.hp.com/hp-linux-imaging-and-printing), the file is
called `hplip-x.xx.xx.run` 

Run it __as ordinary user__ (at some point it will ask adiministrative password).

Use
===

The installed executables start with `hp-` ; there are programs such as `hp-scan` for scanning, and  `hp-config_usb_printer` for USB printer.

To autoconf a network printer:

    hp-setup 172.16.10.13

