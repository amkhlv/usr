Main references
===============

Instructions from [JamieLinux](https://jamielinux.com/docs/openssl-certificate-authority/index.html)


Using Dhall
===========

[Dhall configuration program](https://github.com/jvanbruegge/dhall-openssl)

Notice that SAN should be non-empty, 


Setting up Root CA
==================

__DEPRECATED__ use [Using Dhall]

As described on [JamieLinux](https://jamielinux.com/docs/openssl-certificate-authority/create-the-root-pair.html)


Create an intermediate authority
================================

__DEPRECATED__ , see [Using Dhall]

Again, as described in [JamieLinux](https://jamielinux.com/docs/openssl-certificate-authority/create-the-intermediate-pair.html)

Instead of `intermediate` use some reasonable name such as `myvpn`


Useful commands
===============

__DEPRECATED__ , see [Using Dhall]

Create CA
---------

    openssl req -new -nodes -text -out root.csr -keyout root.key -subj "/CN=root.yourdomain.com"

    chmod og-rwx root.key

    openssl x509 -req -in root.csr -text -days 3650  -extfile /etc/ssl/openssl.cnf -extensions v3_ca  -signkey root.key -out root.crt

Signing certs with it
---------------------

    openssl req -new -nodes -text -out server.csr -keyout server.key -subj "/CN=someserver.yourdomain.com"

    chmod og-rwx server.key

When signing __first time__ have to `-CAcreateserial`:

    openssl x509 -req -in server.csr -text -days 365 -CA root.crt -CAkey root.key -CAcreateserial -out server.crt

__Attention:__ on next invocation, `-CAcreateserial` should be replaced with: `-CAserial root.srl`

SAN
---

    openssl x509  -req -extfile <(printf "subjectAltName=DNS:minisite.ddns.net")   -in server.csr -text -days 365 -CA root.crt -CAkey root.key -CAcreateserial -out server.crt

(but see [Using Dhall])

Examining
---------

    openssl x509 -in some.crt -text -noout

p12
===

This command outputs `x509` on stdout, which can then be handled by `openssl x509`

    openssl pkcs12 -in some.p12 -nodes 
