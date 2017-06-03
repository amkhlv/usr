#!/bin/bash

while read M; do
    sqlite3 ~/a/emails.sqlite "select p from emails where msgid like '%"$M"%'"
done
