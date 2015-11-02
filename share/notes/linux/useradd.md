Use adduser instead
===================

Usually it is advised to use the command `adduser` instead of `useradd` :

    adduser andrei


Creating new user
=================

    useradd -c ircd -g irc -m -d /var/run/ircd -s /usr/sbin/nologin  irc

This will create the new user `irc` with the group `irc` and homedir `/var/run/ircd`. Notice that `-c ircd` is the _comment field_.

The corresponding line in `/etc/passwd` will be like this:

    irc:x:42:42:ircd:/var/run/ircd:/usr/sbin/nologin

where `42:42` is some user and group numbers.

