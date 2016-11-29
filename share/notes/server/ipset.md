
Basic use
=========

    ipset create amkhlv-WAN hash:net,iface

To add interface `eth2`:

    ipset add amkhlv-WAN 0.0.0.0/0,eth2

(Notice that `0.0.0.0/0` matches all IP addresses)

Persistent
==========

    ipset save > /etc/ipset

But then need to write a `systemd` unit to `ipset restore -file /etc/ipset` before `netfilter-persistent`


Using sets in iptables
======================

    *nat

    -A POSTROUTING -m set --match-set amkhlv-WAN dst,dst -j MASQUERADE

    *filter

    -A INPUT -p tcp -m set --match-set amkhlv-cont src,src -m set --match-set amkhlv-dnsmasq dst,dst -j ACCEPT

    -A FORWARD -m set --match-set amkhlv-cont src,src -j ACCEPT
    -A FORWARD -m set --match-set amkhlv-cont dst,dst -m set --match-set amkhlv-WAN src,src -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
