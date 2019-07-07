#!/bin/bash

sqlite3 $HOME/a/emails.sqlite "select json_object('msgid', msgid, 'f', f, 't', t, 'cc', cc, 's', s, 'd', d, 'data', data, 'c', c, 'p', p) from emails where $*"
