#!/bin/sh

extras=""
[ "$1" ] && extras=,$1
lsblk -o NAME,SIZE,PARTLABEL,PARTUUID,UUID,MOUNTPOINT$extras
