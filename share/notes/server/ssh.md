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

and pay attention to:

    Protocol 2
    HostKey /etc/ssh/ssh_host_ed25519_key
    HostKey /etc/ssh/ssh_host_rsa_key

    PermitRootLogin without-password
    
    AllowUsers root
    DenyUsers andrei

    KexAlgorithms curve25519-sha256@libssh.org
    Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
    MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,hmac-ripemd160-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,hmac-ripemd160,umac-128@openssh.com

Notice that we need to create the `Ed25519` key with this command:

    cd /etc/ssh
    ssh-keygen -t ed25519 -f ssh_host_ed25519_key < /dev/null

After setup keys, include these lines:

    PasswordAuthentication no
    ChallengeResponseAuthentication no

    PubkeyAuthentication yes


Port Forwarding
===============

To remember:

1. the number following `-L` is the __local port to be forwarded to somewhere__ ; the number following `-R` is the __remote port to be forwarded to somewhere__

2. the switch `-N` means "do not execute remote command" (you do not obtain shell)

3. the switch `-f` means to execute in background

4. besides port forwarding, `ssh` can also make a `SOCKS` tunnel, with the `-D` switch

Local port forwarding
---------------------

    ssh -L 9000:forbidden.com:80 myname@my-vps-server.com

This opens up the blocked website `forbidden.com` through a remote server `my-vps-server.com`. In particular, if I want to access some service on that very server `my-vps-server.com`
(e.g. `PostgreSQL` port 5432):

    ssh -L 9000:localhost:5432 myname@my-vps-server.com 


Remote port forwarding
----------------------

    ssh -R 9000:localhost:3000 my-name@my-vps-server.com

This connects port (3000 on __remote__) to (9000 on __localhost__)

