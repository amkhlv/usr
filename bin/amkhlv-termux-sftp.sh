#!/bin/bash

read TERMUX_IP < ~/.local/boi/termux

sftp   -P 8022   -i ~/a/keys/for-android_rsa   "$TERMUX_IP"  


