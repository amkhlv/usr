Downloading
===========

The source tarball is [here](https://www.unrealircd.org/download) --- notice that they provide a signature and a checksum.

Creating the user and group `irc`
=================================

    useradd -c ircd -g irc -m -d /var/run/ircd -s /usr/sbin/nologin  irc

This will create the new user `irc` with the group `irc` and homedir `/var/run/ircd`. Notice that `-c ircd` is the _comment field_.

The corresponding line in `/etc/passwd` will be like this:

    irc:x:42:42:ircd:/var/run/ircd:/usr/sbin/nologin

where `42:42` is some user and group numbers.

Installing
==========

Install `libssl-dev`

__Do not__ do anything as `root`. Instead, create a common user (_e.g._ `andrei`) and do everything in `/home/andrei` :

> so, there are two users involved: irc and andrei
> andrei is just for building the Unreal and storing in /home/andrei
> but the Unreal itself will run as the user irc

So, untar the downloaded tarball into `/home/andrei`, go to that directory `UnrealN.N.N.N` and say:

    ./Config

answering questions,  say YES to SSL.

After configuration is complete:

    make

Configuration
=============

    cp doc/example.conf unrealircd.conf

The instructions inside that conf file are very well written.

Running
=======

Don't forget to setup the ownership:

    chown -R irc:irc Unreal*

Notice that the main executable is in `src/ircd`, so we can run it:

    sudo -u irc src/ircd




