#!/bin/bash

DISPLAY=:0.0  XAUTHORITY=~/.Xauthority  build-qt5msgr-Desktop-Debug/qt5msgr outgoing.html incoming.txt &

emacs -nw -q -l init.el outgoing.html incoming.txt


