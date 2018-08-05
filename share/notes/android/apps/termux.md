Installing
==========

Via `F-Droid`, install `Termux` 

Location of data folder
-----------------------

It is `/data/data/com.termux/files/` , in particular `HOME` is `/data/data/com.termux/files/home/` (there is only one user).

Using touch keyboard
====================

Termux uses the `Volume down` button to emulate the `Ctrl` key.

__Exception__ :   `Volume down` + `q` brings up a small horizontal bar on top of 
touch keyboard, containing extra keys such as `Ctrl` and `Alt`.

Access via SSH
==============

    pkg install openssh

Then populate `.ssh/authorized_keys`

To use:

    sshd
    
will listen on Port `8022`

To log to console:

    logcat -s 'syslog:*'

To stop:

    pkill ssh

Termux Android API
==================

    pkg install termux-api

This installs many commands, all starting with `termux-` .
