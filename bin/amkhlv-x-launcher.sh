#!/bin/bash

DSP=:0.0

[ "$DISPLAY" ] && DSP=$DISPLAY

DISPLAY=$DSP XAUTHORITY=/home/andrei/.Xauthority $@

