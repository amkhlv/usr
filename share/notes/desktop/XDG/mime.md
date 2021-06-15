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


How to determine which MIME type a file corresponds to?
=======================================================

Note that `xdg-mime` can also determine the type of the file, _e.g._ :

    xdg-mime query filetype mytext.pdf

return `application/pdf`


Setting the default application
===============================

So far we explained how the set of all possible handlers is defined, using the `.desktop` files. But which one is the default?

This is changed by running `xdg-mime`:

    xdg-mime default tel-handler.desktop x-scheme-handler/tel

To check that it was set:

    xdg-mime query default x-scheme-handler/tel

Problem: many config files
==========================

mimeapps and defaults
---------------------

`xdg-mime` stores settings in `~/.config/mimeapps.list`. 

Other configuration files  can be seen by running:

    XDG_UTILS_DEBUG_LEVEL=2 xdg-mime query default ...

Some of them are:


1. `~/.config/mimeapps.list`

2. `~/.local/share/applications/mimeapps.list`

3. `~/.local/share/applications/defaults.list`

4. ...

XDG_UTILS_DEBUG_LEVEL=2
=======================

is __very useful__, as an environment for `xdg-mime` and other `xdg-`...


update-alternatives
===================

This is another mechanism which sometimes interferes. For example, there is `/usr/bin/x-www-browser` which has to be
updated in some way like this:

    update-alternatives --install /usr/bin/x-www-browser x-www-browser /home/andrei/.local/bin/firefox 100
    update-alternatives --set x-www-browser /home/andrei/.local/bin/firefox

