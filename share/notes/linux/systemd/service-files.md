
Links
=====

[Examples from Arch](https://wiki.archlinux.org/index.php/Systemd/Services)

[Patrakov.blogspot.com](http://patrakov.blogspot.com.br/2011/01/writing-systemd-service-files.html)


Manual pages
============

Common to all unit types are sections `[Unit]` and `[Install]` ; they are documented in:

    man systemd.unit

The `service` type of unit has also a `[Service]` section. It is documented in:

    man systemd.service


Service section
===============


    RemainAfterExit=

        Takes a boolean value that specifies whether the service shall be considered active even when all its processes exited. Defaults to no.

