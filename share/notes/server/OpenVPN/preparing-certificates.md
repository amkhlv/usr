Recommended way: EasyRSA
========================

This should be done everywhere:

1. on the CA machine

2. on each VPN client

3. on the VPN server

The package `easy-rsa` exists in `Debian`, and contains a command `make-cadir` for creating the `easy-rsa/` root folder.

    aptitude install easy-rsa
    cd /some/path
    make-cadir easy-rsa/
    cd easy-rsa

Further instructions are on [Debian wiki](https://wiki.debian.org/OpenVPN)

