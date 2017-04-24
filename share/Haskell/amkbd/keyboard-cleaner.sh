#!/bin/bash

MYPIPE=$HOME/.amkhlv-keyboardpipe.fifo

[ -p "$MYPIPE" ] || exit 1

echo exit > "$MYPIPE" &
MYPID=$!

sleep 0.5

# if somebody ate it, then just exit now:
kill -0 $MYPID || exit 0

# otherwise, clean it up:
cat "$MYPIPE" &
MYPID1=$!
# this should not be needed, but just in case:
sleep 0.1
kill $MYPID1
