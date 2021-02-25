Installing
==========

Via `F-Droid`, install `Termux` 

Location of data folder
-----------------------

It is `/data/data/com.termux/files/` , in particular `HOME` is `/data/data/com.termux/files/home/` (there is only one user).

Termux Android API
==================

    pkg install termux-api

This installs many commands, all starting with `termux-` .

But for them to work, need also to install `Termux:API` from Google Play
and then go to App Info and allow `Change System Settings`. Also, in App Info,
allow Camera, Contacts, Location, Microphone and Phone.

Enable access to Android storage
================================

    termux-setup-storage

This opens permissions, and creates nice symlinks in `~/storage/` to `/storage/emulated/0/...`

Using touch keyboard
====================

Termux uses the `Volume down` button to emulate the `Ctrl` key.

__Exception__ :   `Volume down` + `q` brings up a small horizontal bar on top of 
touch keyboard, containing extra keys such as `Ctrl` and `Alt`.

