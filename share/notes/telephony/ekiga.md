VPN from DiamondCard
====================

There was some problem with conflicting names for tunnels. As I have already `tun0` and `tun1`, I decided to use the name `tun3` for the Diamondcard VPN interface. For that, I needed to edit the file `/etc/openvpn/eu.ovpn` and edit the line `dev tun` to be:

    dev tun3

Firewall and routing
====================

Need to open 5060/udp on tun3

Just to check routing, execute `traceroute sip.diamondcard.us`; it should give something like:

    traceroute to sip.diamondcard.us (46.19.58.41), 30 hops max, 60 byte packets
    1  10.110.210.1 (10.110.210.1)  262.630 ms  262.556 ms  262.523 ms
    2  46.19.58.41.diamondcard.us (46.19.58.41)  262.492 ms  262.460 ms  262.845 ms

(where `10.110.210.1` is the gateway of `tun3`).

Acount configuration with DiamondCard
=====================================

I use the [Diamondcard](https://www.diamondcard.us/exec/prsshow) VoIP service.

The account configuration is as follows:

    Name :   whatever 
    Server:  sip.diamondcard.us
    User:    NNNNN     (this is Account ID, see Account Info top right corner on website)
    User for authentication: NNNNN   (again Account ID)
    Password: Login into Diamondcard, look at Account Info at the right top corner,
              password is PIN code


Audio Configuration
===================

Need some trial and error with configuring the microphone. With my USB microphone,
I had luck with the following: `alsa_input.usb-...` (it is somewhere near the top of the dropdown menu)


Testing and using
=================

To test echo, call:

    sip:441@sip.diamondcard.us

To call Moscow, say:

    sip:+7KKKLLLMMMM@sip.diamondcard.us

