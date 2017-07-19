
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

- `apt-get update && apt-get dist-upgrade`

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

Generate both `en_US.UTF-8 UTF-8` and `pt_BR.UTF-8 UTF-8` and set Default locale for the system environment to `en_US-UTF8`


Machinectl
==========

To list all running containers:

    machinectl list

To login, using `machinectl login`, the `dbus` should be installed in the container.
Also, it does not login to `root`, so first create an ordinary user and then login to it:

    machinectl login Name

To poweroff use __this__ :

    machinectl poweroff Name

Sample systemd unit
===================

With some insight from [here](https://github.com/LudicLinux/LudicLinux.github.io/blob/master/_posts/2017-06-27-Nspawn-Steam-Container.md)


    [Unit]
    Description=Container Arado
    Documentation=man:systemd-nspawn(1)

    [Service]
    DeviceAllow=/dev/dri rw
    ExecStart=/usr/bin/systemd-nspawn --machine Arado  --keep-unit --boot --link-journal=no --directory=/var/lib/container/Arado  --network-veth --bind-ro=/home/andrei/.Xauthority --bind=/tmp/.X11-unix:/root/x11 --bind=/dev/dri:/dev/dri --bind=/dev/shm:/dev/shm --bind=/run/user/1000/pulse:/run/user/host/pulse 
    KillMode=mixed
    Type=notify
    RestartForceExitStatus=133
    SuccessExitStatus=133

    [Install]
    WantedBy=multi-user.target


We are binding `/tmp/.X11-unix/` to `/root/x11` instead of `/tmp/.X11-unix/` , just to work around a [systemd bug](https://github.com/systemd/systemd/issues/4789);
we then need to `/bin/mount -o bind /root/x11 /tmp/.X11-unix` inside the running container.
