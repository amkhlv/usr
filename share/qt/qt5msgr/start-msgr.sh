#!/bin/bash

DISPLAY=:0.0  XAUTHORITY=~/.Xauthority  ./qt5msgr outgoing.html incoming.txt &

emacs -nw -q -l init.el


