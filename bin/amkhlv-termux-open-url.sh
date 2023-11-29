#!/bin/sh

eval $(cat ~/a/Dhall/system.dhall | dhall-to-bash --declare AMKHLV)

ssh   -p 8022   -i ~/a/keys/for-android_rsa   "${AMKHLV[termux]}"   "termux-open-url $1"


