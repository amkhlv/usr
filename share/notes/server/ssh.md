Client
======

Unicode font display foreign characters
---------------------------------------

First read [writeup on locale](../linux/locale.html)

From [Stack Exchange](http://unix.stackexchange.com/questions/16771/foreign-characters-wont-display-in-ssh/16784) :

When running `ssh`, we need to send locale information through the `ssh` connection. For this, add the following lines at the end of `~/.ssh/config`:

    Host *
    SendEnv LC_*

This requires that a suitable `AcceptEnv` directive be present in the server configuration (`/etc/ssh/sshd_config`) (it is by default on `Debian`).



Server
======

Installation
------------

    aptitude install openssh-server

The main configuration files are in the directory /etc/ssh :

    ssh_config : client configuration file

    sshd_config : server configuration file


Configuration
-------------

See [here](https://stribika.github.io/2015/01/04/secure-secure-shell.html)

    vim /etc/ssh/sshd_config

DANGER: socket activation
-------------------------

Apparently, `systemd` introduced __socket activation__ of `sshd`, controlled by the unit: `ssh.socket`

This completely ignores the `ListenAddress` in `/etc/ssh/sshd_config` !

Make sure that this is disabled: `systemctl disable ssh.socket` 



Systemd startup problems
------------------------

Typically, `ssh` will attempt to start before the specified network is ready, and fail.

The solution is to execute `systemctl edit ssh.service` and enter the lines:

    [Unit]
    Wants=network-online.target
    After=network-online.target

(this will __merge__ with the existing `systemd` service file)



Port Forwarding
===============

To remember:

1. the number following `-L` is the __local port to be forwarded to somewhere__ ; the number following `-R` is the __remote port to be forwarded to somewhere__

2. the switch `-N` means "do not execute remote command" (you do not obtain shell)

3. the switch `-f` means to execute in background

4. besides port forwarding, `ssh` can also make a `SOCKS` tunnel, with the `-D` switch

Local port forwarding
---------------------

This creates __outgoing tunnel__ . This means that LOCAL, _i.e._ outgoing, port on my machine gets positioned on some remote machine (as if my
computer grows a huge appendage). In other words "local port forwarding" means "the forwarding of a local port".

    ssh -L 9000:forbidden.com:80 myname@my-vps-server.com

This opens up the blocked website `forbidden.com` through a remote server `my-vps-server.com`.
In particular, if I want to access some service on that very server `my-vps-server.com`
(e.g. `PostgreSQL` port 5432):

    ssh -L 9000:localhost:5432 myname@my-vps-server.com 


Remote port forwarding
----------------------

This creates __incoming tunnel__ .
This means that a remote machine "grows a huge appendage"
and places ITS outgoing port inside MY machine.
In other words "remote port forwarding" means "the forwarding of a remote port".
Although I would rather call it "backwarding".

    ssh -R 9000:localhost:3000 my-name@my-vps-server.com

This connects port (3000 on __remote__) to (9000 on __localhost__)


Formal similarity between local and remote port forwarding
----------------------------------------------------------

In both situations, a part of the command looks like:

    9000:somehost:3000 my-name@otherhost

In both cases we should understand `9000:somehost:3000` as 
"9000 : (somehost:3000)" where `9000` is a port on `otherhost`.
