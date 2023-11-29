#!/bin/sh

eval $(cat ~/a/Dhall/system.dhall | dhall-to-bash --declare AMKHLV)

sshfs  "[${AMKHLV[termux]}]:/storage/emulated/0/DCIM/"  ~/Lapa/dcim   -p 8022   -o "IdentityFile=~/a/keys/for-android_rsa"  

sshfs  "[${AMKHLV[termux]}]:/storage/emulated/0/Download/"  ~/Lapa/download   -p 8022   -o "IdentityFile=~/a/keys/for-android_rsa"  


