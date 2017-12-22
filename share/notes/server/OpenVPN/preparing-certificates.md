Recommended way: EasyRSA
========================

This should be done everywhere:

1. on the CA machine

2. on each VPN client

3. on the VPN server

    git clone https://github.com/OpenVPN/easy-rsa
    cd easy-rsa
    git checkout v3.0.3
    build/build-dist.sh --version=3.0.3

This creates `tgz`, which should be untarred to some location. 

Instructions for use are [available here](https://community.openvpn.net/openvpn/wiki/EasyRSA3-OpenVPN-Howto), we will
recapitulate main points here:


Initialize the CA on some machine
---------------------------------

The CA is on some machine, not necessarily server or client (could be  even totally disconnected from the internet). 

    ./easyrsa init-pki
    ./easyrsa build-ca

On each client and server generate key
--------------------------------------

    ./easyrsa init-pki
    ./easyrsa gen-req MYNICKNAME nopass

This will create the key, and also generate the request. The `.req` file should be `scp`-ed to the CA machine.

Also, while we are on that machine, generate the DH key:

    ./easyrsa gen-dh

Signing the request on the CA machine
-------------------------------------

    ./easyrsa import-req /path/to/received.req UNIQUE_SHORT_FILE_NAME
    
Then, for client request:

    ./easyrsa sign-req client MYNICKNAME
    
And for server request:

    ./easyrsa sign-req server MYNICKNAME


Generate secret Hash-based Message Authentication Code (HMAC)
-------------------------------------------------------------

This is unrelated `EasyRSA`; should be done on each client and server:

    openvpn --genkey --secret /root/easy-rsa/keys/ta.key

This will be used to add an additional HMAC signature to all SSL/TLS handshake packets.

Summary
-------

Each client and server (nice symmetry!) has:

1. `MYNICKNAME.key` --- this one is secret; the way we do things it ends up in `EasyRSA-3.0.3/easyrsa3/pki/private/`

2. `MYNICKNAME.crt`

3. `ta.key`

4. `ca.crt` --- this was copied from `EasyRSA-3.0.3/easyrsa3/pki/` on the CA machine




Manual method
=============

This is <b><span style="color: red;">not recommended, just for illustration</span></b>

Setting up Root and Intermediate authorities
--------------------------------------------

As described [in my writeup](../../PKI/PKI.md)

Use some reasonable name instead of `intermediate`, for example `myvpn`


Preparing certificates and requests
-----------------------------------

There is very little difference between a certificate for a server and a certificate for a client.

### Generate machine key

On a server called "antenna":

    mkdir /root/cert
    cd /root/cert
    openssl genrsa -out antenna.key.pem 2048

### Generate sign request

I think I can use the same `openssl.cnf` as on the [intermediate CA authority](../../PKI/PKI.md).
Just `scp` it to `/root/cert` and use it:

    openssl req -config openssl.cnf -key antenna.key.pem -new -out antenna.csr.pem

and `scp` the resulting `antenna.csr.pem` to that machine which runs the intermediate CA authority

Signing
-------

On the CA authority machine, clone the `easy-rsa` package:

    cd ~/a/git/
    git clone https://github.com/OpenVPN/easy-rsa


Put the `antenna.csr.pem` in `intermediate/csr/` and sign it as follows:

    openssl ca -config intermediate/openssl.cnf -extfile /home/andrei/a/git/easy-rsa/easyrsa3/x509-types/server -notext -days 740 -md sha256 -in intermediate/csr/antenna.csr.pem -out intermediate/certs/antenna.cert.pem

This resulting `antenna.cert.pem` should be `scp`-ed to `/root/cert/` on the server machine.

For the client certificate, the signing is the same, except use `-extfile /home/andrei/a/git/easy-rsa/easyrsa3/x509-types/client`

