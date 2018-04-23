x11vnc
======

Scraping is done by `x11vnc`:

    aptitude install x11vnc

Starting server
---------------

    ssh -t -L 5903:localhost:5903  myhost  ' x11vnc   -localhost   -rfbport 5903   -rfbportv6 -1   -display :0 '

The `-rfbportv6 -1` is needed to restrict listening on `ipv6` to `localhost` 
(there is a [known bug](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=672435) )

