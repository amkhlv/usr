#!/usr/bin/env bash

(
    cd -- "$(dirname "$0")" 
    if [ "$1" = "clean" ] ; then
        rm -rf bin/
        rm -rf include/
        rm -rf lib/
        rm -rf __pycache__/
        rm -rf share/
        rm -rf selenium/
        rm pyvenv.cfg
        rm lib64
    else 
        echo "ERROR: unrecognized command"
    fi
)
