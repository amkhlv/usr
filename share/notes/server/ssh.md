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

### First solution

is to execute `systemctl edit ssh.service` and enter the lines:

    [Unit]
    Wants=network-online.target
    After=network-online.target

(this will __merge__ with the existing `systemd` service file)

### Second solution

Enable __global binding__ which allows binding to non-yet-existing socket:

    echo net.ipv4.ip_nonlocal_bind=1 >> /etc/sysctl.conf


Port Forwarding
===============

To remember:

1. the number following `-L` is the __local port to be forwarded to somewhere__ ; the number following `-R` is the __remote port to be forwarded to somewhere__

2. the switch `-N` means "do not execute remote command" (you do not obtain shell)

3. the switch `-f` means to execute in background

4. besides port forwarding, `ssh` can also make a `SOCKS` tunnel, with the `-D` switch

Local port forwarding
---------------------

From __the man page__: _"Specifies that connections to the given TCP port or Unix socket on the local (client) host are to be forwarded to the given host and port, or Unix socket, on the remote side"_

    ssh -L <tunnel port>:<destination address>:<destination port>  andrei@myserver.com

Connections to `<tunnel port>` on the -<b>L</b>ocal machine  (= `ssh` client)  will be forwarded to `<destination address>:<destination port>`
by means of the "intermediary" `myserver.com` (on which I have an account).

This allows __ssh client__ to connect to some place (destination) via __ssh server__, for example:

    ssh -L 9000:forbidden.com:80  myname@my-vps-server.com

__To remember:__

    -L 9000           :       forbidden.com:80
    
    port 9000                 <dest addr>:<dest port>
    on L-local machine
    (i.e. ssh-client)

This opens up the blocked website `forbidden.com` through a remote server `my-vps-server.com`. I just have
to navigate to `http://localhost:9000`.

The address resolution (_e.g._ `forbidden.com`) happens on `my-vps-server.com`. For example, if I put
`localhost:10000` instead of `forbidden.com:80` then it will forward to port `10000` on the loopback of `my-vps-server.com`.
For example, if I want to access `PostgreSQL` (`localhost:5432`) running on `my-vps-server.com`:


    ssh -L 9000:localhost:5432 myname@my-vps-server.com 

Remote port forwarding
----------------------
From __the man page__: _"Specifies that connections to the given TCP port or Unix socket on the remote (server) host are to be forwarded to the given host and port, or Unix socket, on the local side"_

Should have `GatewayPorts yes` in `/etc/ssh/sshd_config`.

This forwards port `9000` of the -<b>R</b>emote machine (the `ssh` server) to `forbidden.com:3000` using my local machine (the `ssh` client)
as intermediary:

    ssh -R 9000:forbidden.com:3000  my-name@my-vps-server.com

This forwards port `3000` on `forbidden.com` to what? To port `9000` on the loopback of the -<b>Remote</b> `ssh` server.

__To remember:__

    (-R 9000)       :       forbidden.com:3000

    port 900                <dest addr>:<dest port>
    on R-remote machine
    (i.e. ssh-server)

The address resolution (_e.g._ `forbidden.com`) happens on `my-vps-server.com`; for example, if I put `-R 9000:localhost:3000` then
it will forward port 9000 on <b>R</b>emote machine to port `3000` on the `lo` of `my-vps-server.com`.

    
Generalization
--------------

<ul>
<li>Forward port `A` on address `B` available on the -<b>L</b>ocal machine</li>
<li>via `X@Y.Z`</li>
<li>to port `C` on some address `D` :
</ul>

    ssh -L A:B:C:D  X@Y.Z

<ul>
<li>Forward port `A` on address `B` available on the -<b>R</b>emote machine `Y.Z` (where I am `X`)</li>
<li>to port `C` on some address `D` via my local machine:</li>
</ul>

    ssh -R A:B:C:D  X@Y.Z

SOCKS
-----

    ssh -D 10000  X@Y.Z

Proxy via `X@Y.Z`.
