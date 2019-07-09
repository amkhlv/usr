#!/bin/bash

while read M; do
    echo $(sqlite3 ~/a/emails.sqlite "select p from emails where msgid like '%"$M"%'")*
done
