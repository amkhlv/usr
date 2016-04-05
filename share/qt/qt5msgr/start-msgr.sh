#!/bin/bash

[ "$1" ] || { echo ERROR: the argument must be  the absolute path to OUTGOING DIR ; exit 1 ; }

emacs -l init.el --outdir "$1" -nw -q  "$1"/start.html


