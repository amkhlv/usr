#!/bin/env bash

scp setup.sh check.sh requirements.txt $1

(
    cd ..
    rsync -cav --delete --exclude db.sqlite3 auth-redirector/redirector $1/
) 
