#!/bin/bash

DISPLAY=:0.0  XAUTHORITY=~/.Xauthority  build-qt5msgr-Desktop-Debug/qt5msgr outgoing incoming.txt &

emacs -nw -q -l init.el outgoing/hi.html incoming.txt


