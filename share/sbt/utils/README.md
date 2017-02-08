GnuPG
=====

The folder `~/.gnupg` should contain the file `gpg-agent.conf` with single line:

    allow-loopback-pinentry

Also, the file `gpg.conf` should have a line:

    pinentry-mode loopback
