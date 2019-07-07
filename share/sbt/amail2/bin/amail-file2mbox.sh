#!/bin/bash

MBOX=/var/mail/`whoami`
[ "$1" ] && MBOX="$1"

while read F; do
    formail < "$F"* >> "$MBOX"
done

