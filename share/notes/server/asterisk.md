Userful links
=============

Knowledge base
--------------

[http://asterisk.ru/knowledgebase](http://asterisk.ru/knowledgebase)

Security
--------

[How secure is your PBX ? part-1](http://kb.smartvox.co.uk/asterisk/secure-asterisk-pbx-part-1/)

[How secure is your PBX ? part-2](http://kb.smartvox.co.uk/asterisk/secure-asterisk-pbx-part-2/)

Installation
============

    aptitude isntall asterisk

Configuration
=============

Conf files
----------

Configuration happens in `/etc/asterisk` . There are more than 100 configuration files.
However only few have to be modified. It is useful to first make a copy of the modified files, _e.g._ :

    cp sip.conf sip.conf.orig

This way I will know which files I modified, and see the modifications using `diff`.

In particular:

At the end of `modules.conf` I add lines:

    noload => chan_skinny.so
    noload => chan_mgcp.so
    noload => pbx_dundi.so
    noload => chan_iax2.so
    noload => chan_unistim.so

because I think that I dont need those modules

In `rtp.conf` I configure `rtpstart` and `rtpend` to _e.g._ 10100 and 10227 (because I read somewhere that the smallest range to be
configured is 128). This is the port range for RTP. (the actual audio)

In `extensions.conf` I comment the line `include => demo` because I do not want to include demo

Most important is `sip.conf` . I put `allowguest=no` and `udpbindaddr=192.168.88.1` . Also, `sip.conf` is where the actual accounts are
configured, _e.g._ :

    [andrei](public-phone,ulaw-phone)
    type=friend
    secret=.........
    host=dynamic
    [pnydeb](public-phone,ulaw-phone)
    type=friend
    secret=.........
    host=dynamic

IPtables
--------

    -A INPUT -p udp -m state --state NEW -m udp -i tun0 --dport 5060 -j ACCEPT
    -A INPUT -p udp -m state --state NEW -m udp -i tun0 --dport 10100:10227 -j ACCEPT

Running and diagnostic
======================

Asterisk is started by the `SysV` services. While it is already running, it is possible to connect to it and obtain the __shell__ as follows:

    asterisk -rvvvv

The shell has many useful commands, _e.g._ :

1. `sip show peers` --- this is to show currently logged in users

2. `sip show users` --- this is to show all possible users (and their passwords)


Client
======

Firewall configuration
----------------------

Ports: 

1. SIP (signaling) is by default 5060, unless STUN reports a NAT.

2. RTP AUDIO stream is usually 5061/5062 (the first port is the steam itself, the second is for statistics; only the first one really matter. Maybe the second one does improve quality under some circumstances)

3. RTP VIDEO stream is usually 5063/5064 (same as for audio)

So, the `ufw` commands are:

    ufw allow in on tun0 to any port 5060 proto udp
    ufw allow in on tun0 to any port 5061 proto udp

Ekiga and linphone
------------------

Linphone crashes with segfault, so I have to use Ekiga. To call, enter in the address line:

    sip:username@192.168.88.xxx

where `xxx` is the address of the user's machine on the local network.

Notice that the configuration files of Ekiga are in `~/.gconf/apps/ekiga`

