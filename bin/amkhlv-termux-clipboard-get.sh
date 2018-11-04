#!/bin/bash

read TERMUX_IP < ~/.local/boi/termux

ssh   -p 8022   -i ~/a/keys/for-android_rsa   "$TERMUX_IP"   "termux-clipboard-get"


