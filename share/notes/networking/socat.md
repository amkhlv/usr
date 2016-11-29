Plain
=====

Simple file transfer over TCP
-----------------------------

On client:

    echo hithere | socat - TCP4:www.domain.org:11111

On server:

    socat TCP-LISTEN:11111 CREATE:eraseme.txt


Simple file transfer over UDP
-----------------------------

On client:

    echo hithere | socat - UDP:www.domain.org:11111

On server:

    socat UDP-LISTEN:123 CREATE:eraseme.txt


OpenSSL in socat
================

The server is on the receiving end.

Generating certificates
-----------------------

Let us assume that the keys directory on __both__ client and server is `/root/socat-server/`.

    openssl genrsa -out $(hostname).key 2048
    openssl req -new -key $(hostname).key -x509 -days 3653 -out $(hostname).crt

When __answering questions make sure__ that `CommonName` is same as `$(hostname)`:

    cat $(hostname).key $(hostname).crt > $(hostname).pem
    chmod 600 $(hostname).key $(hostname).pem

Copy the trust certificate `SERVER-HOST-HAME.crt`  __to client__ , open channel OK.

Then send `CLIENT-HOST-NAME.crt`  __to server__ , open channel OK.


Running server
--------------

    socat  openssl-listen:4433,reuseaddr,cert=/root/socat-server/$(hostname).pem,cafile=/root/socat-server/CLIENT-HOST-NAME.crt  ...


Running client
--------------

    socat  ...  openssl-connect:SERVER-HOST-NAME:4433,cert=/root/socat-client/$(hostname).pem,cafile=/root/socat-client/SERVER-HOST-NAME.crt

It is __important__ that the remote server is specified by `SERVER-HOST-NAME`, which is the same string as was used as `CommonName` when
[generating certificates](#generating-certificates)

The `hostname` is usually automatically registered by the router. If this is not the case, just enter the desired hostname in the client's `/etc/hosts`.


Receiving file
==============

When receiving file, necessary to use the switch `-u` to avoid the __bad file descriptor__ issue,
as explained [here](http://serverfault.com/questions/768942/socat-create-returning-bad-file-descriptor) :

    socat  -u  openssl-listen:4433,reuseaddr,cert=/root/socat-server/$(hostname).pem,cafile=/root/socat-server/CLIENT-HOST-NAME.crt   CREATE:some-filename.txt



