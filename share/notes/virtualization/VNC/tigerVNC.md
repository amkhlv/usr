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

    passwd

This lets to choose a password, which is saved into `~/.vnc/passwd` (in a slightly obfuscated form...)

The file `~/.vnc/xstartup` contains a sequence of commands which are executed immediately after the start of the tiger's  Xserver.
(Essentially, `xinitrc`). Mine is:

    #!/bin/sh

    xfwm4 &
    xfce4-panel &
    xrdb -merge /etc/X11/app-defaults/XTerm
    xrdb -merge /etc/X11/app-defaults/XTerm-color
    xrdb -merge /home/andrei/.Xdefaults
    export PULSE_SERVER=/run/user/host/pulse/native

    setxkbmap -layout us -print | sed -e 's,\+inet[^+"]*,,' | xkbcomp - $DISPLAY

    uxterm &

    monthly &

    indicator /home/andrei/.config/amkhlv/indicator.xml &

The file `~/.vnc/config` contains a list of flags of the command `Xvnc`.

Foreign layout problems
-----------------------

<a name="cyrillic"></a>

__For cyrillic to function correctly__, as explained [here](https://www.linux.org.ru/forum/general/12531593),
the file `~/.vnc/xstartup` should contain the line:

    setxkbmap -layout us -print | sed -e 's,\+inet[^+"]*,,' | xkbcomp - $DISPLAY

See also [here](https://github.com/TigerVNC/tigervnc/issues/93) and [here](https://github.com/TigerVNC/tigervnc/issues/339),
and [my writeup](cyrillic.md), and eventually [Иван Паскаль](http://pascal.tsu.ru/other/xkb/setup.html).

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

is done __on the client side__ (but probably would not hurt to `setxkbmap -layout ...` on the server side, too, just to make sure to load keysims)

Clipboard
---------

Need to have running `vncconfig --nowin` on the server side, in order to share clipboard.

DPI
---

    xrandr --dpi 89

