#!/bin/bash

WALLPAPERS=/home/andrei/a/other/Ubuntu/wallpapers/parovoz/

LOCKPID=/tmp/amkhlv-xtrlock.pid
FEHPID=/tmp/amkhlv-feh.pid
LOCKLOG=/tmp/amkhlv-xtrlock.log

get_wallpaper () {
    TOTAL="$(find $WALLPAPERS | wc -l)"
    N="$(echo "($TOTAL - 1) * $RANDOM / 32767" | bc)"
    ALLIMAGES=(`find $WALLPAPERS -type f`)
    echo -n ${ALLIMAGES[$N]}
}
if [ $1 == "feh" ] ; then
    DISPLAY=":0" feh --fullscreen `get_wallpaper` &
fi
if [ $1 == "lock"  -a ! -f "$LOCKPID" ] ; then
    DISPLAY=":0" feh --fullscreen `get_wallpaper` &
    echo $! >> "$FEHPID"
    DISPLAY=":0" xtrlock  &
    echo $! >> "$LOCKPID"
    echo "-----------------------------" >> "$LOCKLOG"
    date >> "$LOCKLOG"
    echo "locking" >> "$LOCKLOG"
fi
if [ $1 == "unlock" ]; then
    date >> "$LOCKLOG"
    echo "unlocking" >> "$LOCKLOG"
    for p in $(cat "$LOCKPID"); do 
        echo "killing $p" >> "$LOCKLOG"
        kill $p  
    done
    for q in $(cat "$FEHPID"); do
        kill $q
    done
    rm "$LOCKPID"
    rm "$FEHPID"
fi
