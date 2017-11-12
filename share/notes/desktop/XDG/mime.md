Attention
=========

<b><span style="color:red;">Unfortunately</span></b>, there are [several config files](#XDGMadness)

The set of all possible handlers
================================

Desktop files
-------------

The directory `~/.local/share/applications` should contain a list of `.desktop` files, for example `tel-handler.desktop`:

    [Desktop Entry]
    Version=1.0
    Type=Application
    Exec=/usr/local/lib/amkhlv/tel-handler.sh %u
    StartupNotify=true
    Terminal=false
    Categories=Utility;X-XFCE;X-Xfce-Toplevel;
    MimeType=x-scheme-handler/tel
    Name=Calling Tel
    Comment=Call Tel

Note that it has a line `MimeType=...` which tells that `tel-handler`  __is one of applications which can handle__ `x-scheme-handler/tel` types.

More info is in [my writeup on Desktop files](desktop-entries.md)

Database has to be updated
--------------------------

    update-desktop-database


Setting the default application
===============================

So far we explained how the set of all possible handlers is defined, using the `.desktop` files. But which one is the default?

This is changed by running `xdg-mime`:

    xdg-mime default tel-handler.desktop x-scheme-handler/tel

To check that it was set:

    xdg-mime query default x-scheme-handler/tel


<a name="XDGMadness"></a>

Problem: many config files
==========================

mimeapps and defaults
---------------------

`xdg-mime` stores settings in `~/.local/share/applications/mimeapps.list`. 

<b><span style="color:red;">Problem:</span></b> there is another `mimeapps.list`, located in `~/.config/`.
The `xdg-mime` does not use it. But many other programs use that one. In fact `~/.local/share/applications/mimeapps.list`
is considered deprecated. In fact, people say that `~/.config/mimeapps.list` <b><span style="color:red;">overrides</span></b>
`~/.local/share/applications/mimeapps.list` in many applications (sometimes including even `xdg-mime` itself!)

In other words, there are <b><span style="color:red;">two conflicting</span></b>  configuration files:

1. `~/.config/mimeapps.list`

2. `~/.local/share/applications/mimeapps.list`

Actually, sometimes there is also the <b><span style="color:red;">third one</span></b>:

    ~/.local/share/applications/defaults.list

I guess the right solution would be to somehow synchronize all three, to be on the safe side.
(But symlinking does not work, because all three can be written on!)

mailcap
-------

Actually, <b><span style="color:red;">some applications use</span></b> `~/.mailcap` instead.
Moreover, <b><span style="color:red;">even `xdg-open`</span></b> seems to first read `~/.mailcap` !

Problem: many alternatives to xdg-open
======================================

It is not clear which program actually is involved. There are also `gvfs-open` (for Gnome?) and `exo-open` (for XFCE4).
Try all three. Maybe one of them will work correctly.

How to determine which MIME type a file corresponds to?
=======================================================

Note that `xdg-mime` can also determine the type of the file, _e.g._ :

    xdg-mime query filetype mytext.pdf

return `application/pdf`
