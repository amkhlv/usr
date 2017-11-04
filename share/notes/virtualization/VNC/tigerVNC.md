Installation
============

On client
---------

    aptitude install tigervnc-viewer

On server
---------

    aptitude install tigervnc-standalone-server

Use
===

Setup of server
---------------

Execute:

    passwd

This lets to choose a password, which is saved into `~/.vnc/passwd` (in a slightly obfuscated form...)

The file `~/.vnc/xstartup` contains a sequence of commands which are executed immediately after the start of the tiger's  Xserver.
(Essentially, `xinitrc`)

Launching server
----------------

    vncserver -localhost yes :9

--- this puts `VNC` on `DISPLAY=:9` only listening on `localhost:5909` (need to `ssh -L` it from the client)

Viewing as client
-----------------

As root:

    ssh -L 5909:localhost:5909 myserverhost

Then as ordinary user:

    vncviewer  -MenuKey F1  -PasswordFile /path/to/my/.vnc/passwd  ::5909

The `PasswordFile` parameter should be a path to the `passwd` file which we [configured on the server](#setup-of-server); 
it should be somehow available on the client machine (`sshfs`?).

Misc
====

Keyboard layout switching
-------------------------

is done __on the client side__

Clipboard
---------

Need to have running `vncconfig --nowin` on the server side, in order to share clipboard.


DPI
---

    xrandr --dpi 89

