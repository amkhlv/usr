Installing
==========

I use [OpenVPN for Android from FDroid](https://f-droid.org/en/packages/de.blinkt.openvpn/)

Routing
=======

The usual `push` mechanism does not work.

Need to add a line in the `.ovpn` file, like:

    route 192.168.10.0 255.255.255.0

Running at boot time
====================

