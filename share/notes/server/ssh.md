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



