Introduction
============

TP-Link routers (such as TL-WR940) have TFTP __client__ which is activated on start on __WAN__ interface, 
when the reset button is pressed before power button and kept pressed for several seconds.


Installing TFTP server on computer
==================================

    aptitude install tftpd-hpa

    systemctl start  tftpd-hpa

Preparing image file
====================

For flushing `TL-WR940n` `v5`, the file `lede-17.01.4-ar71xx-generic-tl-wr940n-v4-squashfs-factory.bin` should be copied to the following path:

    /srv/tftp/wr940nv5_tp_recovery.bin


Flushing
========

The TFTP server is __on WAN__ (the blue port) and not on LAN ...

The computer should be on `192.168.0.66`.
Firewall should allow `INPUT` on port `69` (I think this is UDP, I just kept all traffic open)

Press and hold the `RESET` button. Press `POWER` button. Keep the `RESET` pressed for several seconds.

__The RESET button is the hidden one which requires tip of a pen to press__

__Do not confuse it with WiFi setup button__
