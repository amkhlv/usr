#!/bin/bash

[ "$1" ] || { 
    echo "Use: $0 location"
    echo "where location is typically server:path (or just local path)"
    exit 1;
}

LOCATION="$1"

scp init.el start-msgr.sh incoming.txt outgoing.html build-qt5msgr-Desktop-Debug/qt5msgr "$LOCATION"


