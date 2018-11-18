General format
==============

    tcpdump -i eth0 <expression>

Examples
========

    tcpdump   -i lo   udp
    
    tcpdump   -i lo udp port 80

General syntax of expressions
=============================

The filter expression consists of one or more __primitives__
which can be combined by `and` or `or` , and negated using `not` or `!` , for example:

    host foo and not port ftp and not port ftp-data

Standard primitives
-------------------

__Primitives__ usually consist of an id (name or number) preceded by one or more __qualifiers__

There are  three  different  kinds  of __qualifiers__ :

1. __type__    qualifiers  say  what kind of thing the id name or number refers to.
   Possible types are host, net , port and portrange.
   E.g., `host foo`, `net 128.3`, `port 20`, `portrange 6000-6008`.  If there is no type qualifier, `host` is assumed.
              
2. __dir__     qualifiers specify a particular transfer direction to and/or from id.  
   Possible directions are `src`, `dst`, `src or dst`, `src and dst`, `ra`, `ta`, `addr1`, `addr2`, `addr3`, and `addr4`.
   E.g., `src  foo`, `dst net 128.3`, `src or dst port http`.  If there is no dir qualifier, `src` or `dst` is assumed.
   The ra, ta, addr1, addr2, addr3, and addr4 qualifiers are only valid for IEEE 802.11 Wireless LAN link layers.

3. __proto__   qualifiers restrict the match to a particular protocol.  
   Possible protos are: `ether`, `fddi`, `tr`, `wlan`, `ip`, `ip6`, `arp`, `rarp`, `decnet`, `tcp` and `udp`.  
   E.g., `ether src foo`, `arp net 128.3`, `tcp port 21`, `udp portrange 7000-7009`, `wlan addr2 0:2:3:4:5:6`.  
   If there is no proto qualifier, all protocols consistent with the type are assumed.

Special primitives
------------------

there are some special __primitive__ keywords that don't follow the pattern: 

1. `gateway` 
2. `broadcast` 
3. `less` 
4. `greater`  
5. arithmetic  expressions.

Available expressions
=====================

The list of all expression
--------------------------

can be obtained by running:

    man pcap-filter

Some useful expressions
-----------------------

    dst host <host>
    src host <host>
    host <host>     <-- this one selects packets with either dst or src being <host>
    port <port>     <-- same here for port...
    gateway <host>  <-- this one selects packets which use <host> as gateway
    tcp, udp, icmp  <-- abbreviations for:  proto <p>
    src net 192.168 <-- only from 192.168.xxx.xxx 

Example: Extract HTTP User Agents
---------------------------------

    tcpdump -nn -A -s1500 -l | grep "User-Agent:"

Capturing into file and WireShark
=================================

    tcpdump ... -w capturefile.pcap

and then use WireShark __as an unprivileged user__ to analize:

    wireshark capturefile.pcap

