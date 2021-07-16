Member of groups
================

    lp dialout audio video plugdev bluetooth pulse pulse-access 

Command line
============

    bluetoothctl

--- opens an interactive "shell". That shell has several important commands.

Security first
--------------

The __most important one__ is:

    [...]# show

It shows __my__ adaptor. For security reasons, it should not be discoverable.
Moreover, when I am not pairing, it should not be pairable either.
So, I should have:

    Discoverable: no
    Pairable: no

--- in the output of `show`.

If this is not the case, I need to say:

    [...]# discoverable off
    [...]# pairable off

To list all available commands (for pairing _etc_):

    [...]# help

Pairing devices
---------------

First scan:

    [...]# scan on

(this will take some time). Then, to list the discovered devices:

    [...]# devices

Devices are listed by MAC address. Tab-completion works on MAC.
The commands for pairing are `pair` and `trust`.

(`connect` comes after `pair`...)


