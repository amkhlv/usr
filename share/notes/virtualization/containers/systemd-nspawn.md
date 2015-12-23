
Restriction on the machine name
===============================

Machine name should not be too long. (This is a bug)


Checklist for creating the new container
========================================

We assume that this is in `/var/lib/container/Name`

Debootstrap
-----------

     debootstrap jessie /var/lib/container/Name http://ftp.br.debian.org/debian

Prepare
-------

     systemd-nspawn -M Name -D /var/lib/container/Name

Then do the following:

- replace `etc/apt/sources.list` with something reasonable

- `passwd` to create the root password

- `apt-get update && apt-get dist-upgrade` (althought this should not be necessary)

- `adduser andrei` to create the new user (for logging in via `machinectl login`)

- `apt-get install dbus` (for logging in via `machinectl login`)


Firewall
--------

Create `etc/iptables` and copy some `rules.v4` and `rules.v6` there


First Boot
----------

To boot:

    systemd-nspawn --machine Name --quiet --boot --link-journal=try-guest --directory=/var/lib/container/Name --network-macvlan=eth0 --network-veth

(If run via `systemd` service , add `--keep-unit`)

    apt-get install iptables-persistent

To poweroff:

    machinectl poweroff Name

(this will automatically shutddown interfaces)

Locales
-------

    dpkg-reconfigure locales

and set `LOCALE` to `en_US-UTF8`


Machinectl
==========

To list all running containers:

    machinectl list

To login, using `machinectl login`, the `dbus` should be installed in the container.
Also, it does not login to `root`, so first create an ordinary user and then login to it:

    machinectl login Name

To poweroff use __this__ :

    machinectl poweroff Name

