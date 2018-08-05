# Useful links

Concise instructions:
[http://d.stavrovski.net/blog/post/how-to-install-and-set-up-openvpn-in-debian-7-wheezy](http://d.stavrovski.net/blog/post/how-to-install-and-set-up-openvpn-in-debian-7-wheezy)

Detailed instructions:
[https://wiki.archlinux.org/index.php/OpenVPN](https://wiki.archlinux.org/index.php/OpenVPN)

Official manual:
[https://community.openvpn.net/openvpn/wiki/Openvpn23ManPage](https://community.openvpn.net/openvpn/wiki/Openvpn23ManPage)

# Make sure that VPS supports TUN

1. Got to the VPS Control Panel and enable `TUN`

2. Reboot the server

3. On the server, execute:

     test ! -c /dev/net/tun && echo openvpn requires tun support || echo tun is available

to see if it works


## Configuration of the server

__Sample__ configuration file (Comments are preceded with '#' or ';' ) :

    /usr/share/doc/openvpn/examples/sample-config-files/server.conf.gz

This should be `gunzip`-ed and copied to `/etc/openvpn/server/NAME.conf` and edited.

Then the server is started by:

    systemctl start openvpn-server@NAME.service

The recommended __network topology__ is `subnet` :

    topology subnet

## Configuration of the client

__Sample__ configuration file (Comments are preceded with '#' or ';' ) :

    /usr/share/doc/openvpn/examples/sample-config-files/client.conf

This should be `gunzip`-ed and copied to `/etc/openvpn/client/NAME.conf` and edited.

Then the server is started by:

    systemctl start openvpn-client@NAME.service

### Renaming TUN/TAP interfaces

In `server.conf`, replace `dev tun` with two lines:

    dev myDevName-tun
    dev-type tun

# CCD files

The name of the CCD file should be the `CommonName` of the client's certificate.

Sample CCD file consists of one line (remember that we choosen __network topology__ to be __subnet__ ):

    ifconfig-push 192.168.88.113 255.255.255.0

# Routing

If we want to route to a subnet of one of the clients, we have to communicate to `OpenVPN` on which client that subnet is.
This is done by the `iroute` line in the CCD file. For example, if a CCD file of a client has a line:

    iroute 192.168.9.0 255.255.255.0

then `OpenVPN` will know that the subnet `192.168.9.0/24` is available on that client. We can route to it on __another client__,
by putting into another client's CCD file:

    push "route 192.168.9.0 255.255.255.0 192.168.88.113"

where `192.168.88.113` is that VPN address of that first client (which has the subnet `192.168.9.0/24`; dont forget to
enable `net.ipv4.ip_forward=1` in `/etc/sysctl.conf` there, and also `MASQEURADE` in `*nat`'s `POSTROUTING` and `ACCEPT` in `*filter`'s `FORWARD` in its `iptables`).

## Viewing the VPN status

On the server:

    tail -f -n40 /etc/openvpn/openvpn-status.log

--- this shows the number of connected hosts _etc._

