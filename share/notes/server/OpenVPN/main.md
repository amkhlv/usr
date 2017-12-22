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

## Configuration of the client

__Sample__ configuration file (Comments are preceded with '#' or ';' ) :

    /usr/share/doc/openvpn/examples/sample-config-files/client.conf

This should be `gunzip`-ed and copied to `/etc/openvpn/client/NAME.conf` and edited.

Then the server is started by:

    systemctl start openvpn-client@NAME.service

# Routing all traffic on a client through the server

## On the client

add a line:

    redirect-gateway def1

to the client configuration file

## On the server

    iptables -t nat -I POSTROUTING -o venet0 -s 10.8.0.0/24 -j MASQUERADE
    iptables -I FORWARD -i tun0 -o venet0 -s 10.8.0.0/24 -m conntrack --ctstate NEW -j ACCEPT
    iptables -I FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT


# Static IP addresses

Remember that in my server configuration file I have a line:

    client-config-dir ccd

This means that I have to setup a directory `/etc/openvpn/ccd/` (\`\`client configuration directory''):

    mkdir /etc/openvpn/ccd

Remember that when we created the client certifiates, we were asked for __Common Name__ (`CN`)
Here it goes:

    vi /etc/openvpn/ccd/CommonName

(where `CommonName` is that Common Name!) Inside that file should be:

    ifconfig-push 192.168.88.113 192.168.88.112

(Here `192.168.88.113` is the IP address I want for my client CommonName, and `192.168.88.112` is the peer interface address.)

# Running OpenVPN

On the client, go to `/etc/openvpn` and say:

    openvpn servername.conf

## Viewing the VPN status

On the server:

    tail -f -n40 /etc/openvpn/openvpn-status.log

--- this shows the number of connected hosts _etc._

# No /dev/net/tun

    mkdir /dev/net
    mknod /dev/net/tun c 10 200

