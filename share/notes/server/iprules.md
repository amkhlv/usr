Creating new routing table
==========================

    echo 11 viahome >> /etc/iproute2/rt_tables

Adding routes to our new table
==============================

    ip route add default via 10.8.0.5 dev tap0  table viahome
    ip route add 10.8.0.0/24 dev tap0 table viahome

    ip route flush cache

To see all from the new table:

    ip route show table viahome

Creating new lookup rule
========================

    ip rule add from 192.168.122.0/24 lookup viahome

To see all the rules:

    ip rule
