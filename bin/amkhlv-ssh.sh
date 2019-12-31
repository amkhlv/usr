#!/usr/bin/env bash

[ "$1" ] || { 
    echo error: '$1' should be configuration name, see '~/.config/amkhlv/ssh/'  ; 
    exit 1 ; 
    }

[ -f ~/.config/amkhlv/ssh/$1.json ] || {
    echo error: no config file ~/.config/amkhlv/ssh/$1.json ;
    exit 1 ;
}

A="$(cat ~/.config/amkhlv/ssh/$1.json  | jq '.args' -re)"
XA=$(echo -n $?)
H=$(cat ~/.config/amkhlv/ssh/$1.json  | jq '.host' -r)
C="$(cat ~/.config/amkhlv/ssh/$1.json  | jq '.command' -re)"
XC=$(echo -n $?)

case "${XA}${XC}" in

    00) echo "ssh $A $H \"$C\""
        ssh $A $H "$C"
        ;;
    01) echo "ssh $A $H"
        ssh $A $H        
        ;;
    10) echo "ssh $H \"$C\""
        ssh $H "$C"
        ;;
    11) echo "ssh $H"
        ssh $H
        ;;

esac


