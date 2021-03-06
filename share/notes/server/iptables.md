# General comments

## Summary

Maybe one should first do some basic configuration using the command line tool `iptables`, and then
fine-tune the files  `/etc/iptables/rules.v4` and `/etc/iptables/rules.v6` which can be edited 
directly and loaded by the commands:

     iptables-restore < /etc/iptables/rules.v4
     ip6tables-restore < /etc/iptables/rules.v6

This requires:

    aptitude install iptables-persistent

We also use `ulogd2`:

    aptitude install ulogd2


## Example of configuration using the command line tool `iptables`

    iptables -P INPUT ACCEPT
    iptables -F
    iptables -A INPUT -i lo -j ACCEPT
    iptables -A INPUT -p icmp --icmp-type any -j ACCEPT
    iptables -A INPUT -p tcp ! --syn -m state --state NEW -j DROP
    iptables -A INPUT -f -j DROP
    iptables -A INPUT -p tcp --dport 22 -j ACCEPT
    iptables -A INPUT -p tcp --tcp-flags ALL ALL -j DROP
    iptables -A INPUT -p tcp --tcp-flags ALL NONE -j DROP
    iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
    iptables -A INPUT -j NFLOG --nflog-group 0 --nflog-prefix "--- Break in Attempt ---"
    iptables -P INPUT DROP
    iptables -P FORWARD DROP
    iptables -P OUTPUT ACCEPT

    iptables -L -v

    ip6tables -P INPUT ACCEPT
    ip6tables -F
    ip6tables -A INPUT -i lo -j ACCEPT
    ip6tables -A INPUT -p ipv6-icmp -j ACCEPT
    ip6tables -A INPUT -p tcp -m tcp ! --tcp-flags FIN,SYN,RST,ACK SYN -m state --state NEW -j DROP
    ip6tables -A INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT
    ip6tables -A INPUT -m state --state INVALID -j DROP
    ip6tables -P INPUT DROP
    ip6tables -P FORWARD DROP
    ip6tables -P OUTPUT ACCEPT

    ip6tables -L -v


__Removing__ the rule is the same as creating except instead of `-A` use `-D`

## Saving iptables

First we need to 

    aptitude install iptables-persistent

Then, the command:

    /etc/init.d/iptables-persistent save

saves into  `/etc/iptables/rules.v4` and `/etc/iptables/rules.v6`, which are then loaded on startup.

After setting up iptables as described above, say:

     /etc/init.d/iptables-persistent save

It saves into  `/etc/iptables/rules.v4` and `/etc/iptables/rules.v6`; it it then loaded on startup

The two files  `/etc/iptables/rules.v4` and `/etc/iptables/rules.v6` can be edited directly, and then:

    iptables-restore < /etc/iptables/rules.v4
    ip6tables-restore < /etc/iptables/rules.v6

Very nice intro: [http://wiki.centos.org/HowTos/Network/IPTables](http://wiki.centos.org/HowTos/Network/IPTables)

# ip4

The command `iptables-save` It saves

## Basic example of the file `/etc/iptables/rules.v4`

For example:

    *filter
    :INPUT DROP [12:2243]
    :FORWARD DROP [0:0]
    :OUTPUT ACCEPT [0:0]
    -A INPUT -i lo -j ACCEPT
    -A INPUT -p icmp --icmp-type any -j ACCEPT
    # Force SYN checks
    -A INPUT -p tcp ! --syn -m state --state NEW -j DROP
    # Drop all fragments
    -A INPUT -f -j DROP
    # Drop XMAS packets
    -A INPUT -p tcp --tcp-flags ALL ALL -j DROP
    # Drop NULL packets
    -A INPUT -p tcp --tcp-flags ALL NONE -j DROP
    -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
    COMMIT

The first line (starting with the asterisk) says that we are going to configure the table `filter`

The next three lines (starting with the column) declares the three chains with their default policies.
The numbers \[...:...\] are for debug/informations purpose only. Leave them at their current value.

## Example configuration for a router

Suppose `eth0` is WAN and `eth1` is LAN

    *filter
    :INPUT DROP [12:2243]
    :FORWARD DROP [0:0]
    :OUTPUT ACCEPT [0:0]
    ........
    -A FORWARD -s 192.168.3.0/24 -i eth1 -j ACCEPT
    -A FORWARD -d 192.168.3.0/24 -i eth0 -j ACCEPT
    COMMIT
    *nat
    :PREROUTING ACCEPT [2338:473784]
    :INPUT ACCEPT [0:0]
    :OUTPUT ACCEPT [16:1364]
    :POSTROUTING ACCEPT [2:298]
    -A POSTROUTING -o eth0 -j MASQUERADE
    COMMIT


__Dont forget__ to edit the file `/etc/sysctl.conf` to have:

    net.ipv4.ip_forward = 1

(It is typically commented out; just uncomment it.)
To change to take effect immediately, execute: `sysctl -p /etc/sysctl.conf`

A very nice writeup can be found [here](http://www.karlrupp.net/en/computer/nat_tutorial)

# ip6

I am not sure if I am doing it right. For now, I just want to block everything except the loopback

    *filter
    :INPUT DROP [412:100176]
    :FORWARD DROP [0:0]
    :OUTPUT ACCEPT [32:5472]
    -A INPUT -i lo -j ACCEPT
    -A INPUT -p ipv6-icmp -j ACCEPT
    -A INPUT -p tcp -m tcp ! --tcp-flags FIN,SYN,RST,ACK SYN -m state --state NEW -j DROP
    -A INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT
    -A INPUT -m state --state INVALID -j DROP
    COMMIT


# Logging with ULOG (NFLOG)

We use `ulogd2`. It is configured in `/etc/ulogd.conf`

The configuration file is very complicated, for us it is important to have uncommented one of the "stack" lines, e.g.:

    stack=log1:NFLOG,base1:BASE,ifi1:IFINDEX,ip2str1:IP2STR,print1:PRINTPKT,emu1:LOGEMU

Then important sections are:

    [log1]
    ...
    group=0
    ...

(which determines `-j NFLOG --nflog-group 0`) and:

    [emu1]
    file="/var/log/ulog/syslogemu.log"

(which determines the location of the log file)

# MANGLE FWMARK

    *mangle
    -A OUTPUT ...  -j MARK --set-mark 2
    COMMIT
