#!/bin/bash

MYPIPE=$HOME/.amkhlv-keyboardpipe.fifo

[ -p "$MYPIPE" ] || ( mkfifo "$MYPIPE" ; chmod 600 "$MYPIPE" )

X="$(cat "$MYPIPE")"

if [ "$( echo "$X" | grep '^go$' )" ] ; then
    DISPLAY=:0.0 XAUTHORITY=/home/andrei/.Xauthority amkbd 
else
    exit 0
fi
