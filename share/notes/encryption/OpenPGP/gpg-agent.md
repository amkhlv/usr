Enable in GPG
=============

In the file `~/.gnupg/gpg.conf` add a line:

    use-agent


Configuration
=============

The configuration file is `~/.gnupg/gpg-agent.conf`. One possibility:

    pinentry-program /usr/bin/pinentry
    display :9
    default-cache-ttl-ssh 86400
    max-cache-ttl-ssh 86400

pinentry-program
----------------

In fact `/usr/bin/pinentry` is a symlink to `/etc/alternatives/pinentry` which is set up by:

    update-alternatives --config pinentry

For example, one can:

    aptitude install pinentry-qt

cache-ttl-ssh
-------------

This is for how long the passphrase will be remembered (in seconds).





