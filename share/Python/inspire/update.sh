#!/usr/bin/env bash

(
    cd -- "$(dirname "$0")" 
    source bin/activate
    pip install -r requirements.txt
)
