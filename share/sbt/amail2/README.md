Config file
===========

The file `~/.config/amkhlv/amail.conf` should contain:

    maildirs = ["/path/to/maildir1/cur/", "/path/to/maildir2/cur/", ...]
    
    dbfiles  = "/home/andrei/a/emails.sqlite"

Indexing
========

`amail-index.sh` without any arguments indexes everything (it makes sense to first `sqlite3 ~/a/emails.sqlite "delete from emails; vacuum;"`)

`amail-index.sh 30` indexes all mail files with `mtime` less than 30 days

Database schema
===============

The file `~/a/emails.sqlite` should have:

    CREATE TABLE EMAILS (msgid TEXT primary key, f TEXT, t TEXT, cc TEXT, bcc TEXT, s TEXT, d TEXT, data TEXT, c TEXT, p TEXT not null);

where:

    msgid: Message ID

    f:     From
    
    t:     To
    
    cc:    Cc
    
    bcc:   Bcc
    
    s:     Subject
    
    d:     yyyymm (derived from Date)
    
    data:  Date
    
    c:     Content-Type, ¶-separated
    
    p:     Email file path, withouth a tail (tails start with `:` and are short sequences of letters and numbers)

Examples
========

    echo aaa@example.com | amail-msgid2file.sh | amail-file2mbox.sh

    amail-select-json.sh where f like "'%fapes%'" and s like "'%[FAPESP]%'" and d like 201906 | jq -r .msgid | amail-msgid2file.sh | xargs -I% ln -s % ~/maildirs/amail/new/
