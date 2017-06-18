#!/usr/bin/env bash

MDPATH="$(readlink -f "$1")"

(
    cd -- "$(dirname "$(readlink "$0")")"
    source bin/activate
    python3 md2html.py "$MDPATH"
) 

