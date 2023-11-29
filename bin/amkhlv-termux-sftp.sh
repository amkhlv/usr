#!/bin/sh

eval $(cat ~/a/Dhall/system.dhall | dhall-to-bash --declare AMKHLV)

sftp   -P 8022   -i ~/a/keys/for-android_rsa   "${AMKHLV[termux]}"  


