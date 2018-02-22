Setting up VPN client
=====================

    opkg install openvpn-openssl luci-app-openvpn 

Then, it is better to configure manually. The file `/etc/config/openvpn` should contain lines:

    config openvpn 'my_client'
            option config '/etc/openvpn/my.ovpn'
            option enabled '1'

It seems that the name should be of the form `SOMEWORD_client`, like that, with underscore!

It is in LuCI under `Services` tab.

Routing through it
==================

Netfilter on the router
-----------------------

The VPN should be of layer-2 type, _i.e._ using `TAP`

In `/etc/config/network` should be lines:

    config interface 'myvpn'
            option proto 'none'
            option ifname 'tap0'

In `/etc/config/firewall` :

    config zone
            option name 'myvpnzone'
            option input 'ACCEPT'
            option output 'ACCEPT'
            option forward 'ACCEPT'
            option network 'myvpn'

    config forwarding
            option src 'myvpnzone'
            option dest 'wan'

This should be enough for routing to happen.

Doing this often requires __restarting the OpenVPN service on the server__


Routing table on the main computer
----------------------------------

See [writeup on IPrules](../server/iprules.md)

