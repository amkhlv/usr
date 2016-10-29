OpenSSL in socat
================

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

    socat  ...  openssl-connect:server.domain.org:4433,cert=/root/socat-client/$(hostname).pem,cafile=/root/socat-client/SERVER-HOST-NAME.crt

