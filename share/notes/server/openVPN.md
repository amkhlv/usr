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

# Generate certificates

Both client and cerver certificates are generated on the server

## Server certificates

    cd /usr/share
    find easy-rsa/ -print0 | cpio -0pmad ~/
    cd ~/easy-rsa
    cp vars{,.orig}
    ./clean-all

Edit `vars` ; notice that `PKCS11_MODULE_PATH` and `PKCS11_PIN` are unimportant.
`KEY_OU` stands for \`\`organizational unit''. Then:

    source ./vars

To generate the CA certificate:

    ./build-ca

This creates `keys/ca.key` and `/keys/ca.crt`.

Generate the server certificate:

    ./build-key-server servername

    Sign the certificate? [y/n]:y
    1 out of 1 certificate requests certified, commit? [y/n]y

Make sure that the __server name__ (Common Name when running the script) __is unique__.

Generate the Diffie-Hellman PEM certificate:

    ./build-dh

Generate secret Hash-based Message Authentication Code (HMAC)

    openvpn --genkey --secret /root/easy-rsa/keys/ta.key

This will be used to add an additional HMAC signature to all SSL/TLS handshake packets.

# Client certificates

First of all, if we have not done it yet, we have to source `vars` :

    source ./vars

Now build the key

    ./build-key clientname

In particular it will ask Common Name. This common name will be important later when setting up connection.
Choose it in a unique way, better without spaces. Notice that the default Common Name will be suggested as `clientnatm`.
This is the right choice.

# Deploy the certificates

### On server

The following files are needed on server:

- Public:

        ca.crt servername.crt dh2048.pem 

- Private:

        servername.key ta.key 

Let us put them in `/etc/openvpn/certs/`:

    mkdir /etc/openvpn/certs/
    cp -pv /root/easy-rsa/keys/{ca.crt,servername.{crt,key},ta.key,dh2048.pem} /etc/openvpn/certs/

### On client

The following files are needed on client:

- Public:

        clientname.crt ca.crt

- Private:

        clientname.key ta.key

I put them all into `/root/openvpn/servername/`:

    mkdir /root/openvpn/servername
    cd /root/openvpn/servername
    scp server:/root/easy-rsa/keys/{clientname.{crt,key},ca.crt,ta.key} ./

## Configuration file on the server

__Sample__ configuration file (Comments are preceded with '#' or ';' ) :

    /usr/share/doc/openvpn/examples/sample-config-files/server.conf.gz

It is `/etc/openvpn/server.conf` (but it is better to just fill-in the above-mentioned sample file):

    port 1194
    proto udp
    dev tun

    ca /etc/openvpn/certs/ca.crt
    cert /etc/openvpn/certs/servername.crt
    key /etc/openvpn/certs/servername.key
    dh /etc/openvpn/certs/dh2048.pem
    tls-auth /etc/openvpn/certs/ta.key 0

    server 192.168.88.0 255.255.255.0
    ifconfig-pool-persist ipp.txt

    push "dhcp-option DNS 8.8.8.8"
    push "dhcp-option DNS 8.8.4.4"

    client-to-client
    keepalive 10 120

    cipher AES-256-CBC # to see all, run: openvpn --show-ciphers
    comp-lzo

    max-clients 10

    user nobody
    group nogroup

    persist-key
    persist-tun

    log-append openvpn.log
    status openvpn-status.log
    verb 5
    mute 20

    client-config-dir ccd



This last line `client-config-dir ccd` is important, it says that additional conf of
individual clients (such at their static IP) is specified in `/etc/openvpn/ccd/`

If I want __all traffic__ to go through the VPN server, I need to add the line:

    push "redirect-gateway def1 bypass-dhcp"

But I probably dont need this. 

## Configuration file on the client

Maybe I should install `resolvconf` :

    aptitude install resolvconf

But I had problems with `resolvconf` on live CD, so I do not use it.

The configuration file is `/etc/openvpn/servername.conf`; again, a nice sample
file is in `/usr/share/doc/openvpn/examples/sample-config-files/client.conf`

    client
    remote myopenvpnsite.com
    ca /root/openvpn/servername/ca.crt
    cert /root/openvpn/servername/clientname.crt
    key /root/openvpn/servername/clientname.key
    ns-cert-type server
    cipher AES-256-CBC # to see all, run: openvpn --show-ciphers
    comp-lzo yes
    dev tun
    proto udp
    tls-auth /root/openvpn/servername/ta.key 1
    ;nobind
    auth-nocache
    persist-key
    persist-tun
    user nobody
    group nogroup

Comments:

1. The files `ca.crt clientname.crt clientname.key ta.key` are those which I downloaded from the server.)

2. Notice that the `nobind` option is commented; otherwize the client will bind to a __random local port__ (which I find ugly)

Additional lines possible. After `script-security 2` :

    up /etc/openvpn/update-resolv-conf
    down /etc/openvpn/update-resolv-conf

\--- this is in case I want to do `resolvconf`. Generally speaking `up` and `down` parameters could
be arbitrary scripts (e.g. restarting `sshd`). Dont forget to make them executable!

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

(Here `192.168.88.113` is the IP address I want for my client CommonName, and `192.168.88.112` is the peer address. Not sure
what it is, but it is NOT the server address.)

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

