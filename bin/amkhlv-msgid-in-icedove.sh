#!/bin/bash


for E in $@; do

    COUNT="$(sqlite3 ~/a/maildirs/mymail.db 'select count(*) from mail where messageid like '"'%$E%'")"
    if [ $COUNT -gt 0 ]; then
        FROM_SQL="$(sqlite3 ~/a/maildirs/mymail.db "select file from mail where messageid like '%$E%'")"
        SIZE=${#FROM_SQL}
        if [ $SIZE -gt 12 ] ; then
            GLOB_OUT="$(ls ${FROM_SQL}*)"
            echo $GLOB_OUT
            for a in $GLOB_OUT; do
                cp $a ~/.amkhlv-emails/"'""$(basename $a)""'".eml && icedove -file ~/.amkhlv-emails/"'""$(basename $a)""'".eml &
            done
        else
            echo "ERROR: SMALL SIZE: " $FROM_SQL
        fi
    fi
done



