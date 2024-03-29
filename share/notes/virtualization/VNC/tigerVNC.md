Installation
============

On client
---------

    aptitude install tigervnc-viewer

On server
---------

    aptitude install tigervnc-standalone-server

Server
======

Setup of server
---------------

Execute:

    vncpasswd

This lets to choose a password, which is saved into `~/.vnc/passwd` (in a slightly obfuscated form...)

__For XFWM4 work__ need to first execute as root:

    loginctl enable-linger USERNAME
    aptitude install dbus-x11

Launching server
----------------

    vncserver -localhost yes :9

--- this puts `VNC` on `DISPLAY=:9` only listening on `localhost:5909`


Client
======

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

DPI
---

    xrandr --dpi 89

