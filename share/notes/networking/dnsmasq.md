
Config
======

I want to have a possibility to run `libvirt`, so I need `bind-dynamic` configuration of `dnsmasq` :

    interface=ve-Mycontainer
    bind-dynamic

(The option `bind-dynamic` is often undocumented)

A nice discussion is [here](http://lists.thekelleys.org.uk/pipermail/dnsmasq-discuss/2012q4/006525.html)
and [here](http://unix.stackexchange.com/questions/256061/is-libvirt-dnsmasq-exposed-to-the-network-if-i-run-fedora-without-a-firewall)

Firewall
========

Open __on LAN__ port 53 for both `TCP` and `UDP`, and port 67 for `UDP`

