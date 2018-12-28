#!/bin/bash

MYPIPE=$HOME/.amkhlv-keyboardpipe.fifo

[ -p "$MYPIPE" ] || ( mkfifo "$MYPIPE" ; chmod 600 "$MYPIPE" )

X="$(cat "$MYPIPE")"

#date >> /tmp/amkhlv-xvkb-log.txt
#cat >> /tmp/amkhlv-xvkb-log.txt

if [ "$( echo "$X" | grep '^go$' )" ] ; then
    amkbd 
elif [ "$( echo "$X" | grep '^exit$' )" ] ; then
    exit 0
else
    exit 0
fi



