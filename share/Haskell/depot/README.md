Installing dependencies
=======================

As root
-------

    aptitude install libncurses5-dev

Then install `stack` as explained on the [FPComplete web site](http://docs.haskellstack.org/en/stable/install_and_upgrade/#debian)
__Do not install__ `ghc` by `aptitude` (because it will be installed locally by `stack`).

As ordinary user
----------------

To install `ghc` into `~/.stack/programs/x86_64-linux/` run:

    stack setup


Building
========

    stack build

Configuration
=============

SQLite file generation
----------------------

    stack runghc app/SetupUserDB.hs

It will be called `users.sqlite`. I can rename it later; the path to the `sqlite` file is configured in `depconf.yaml` (see below).

Password hashing
----------------

The `password` field in the `user` table contains the password hash, which can be prepared as follows:

    stack runghc app/MakeHash.hs

Systemd
-------

`depot@.service` should contain:

    [Service]
    WorkingDirectory=/var/www/depot
    ExecStart=/usr/local/bin/depot-exe %i.yaml
    User=www-data

Nginx
-----

    location /depot/ {
        rewrite /depot(.*) $1  break;
        proxy_pass http://127.0.0.1:21212;
        proxy_redirect     off;
        proxy_set_header   Host $host;
    }

depconf.yaml
------------

    port: 21212
    dbfile: users.sqlite
    root: "https://mysite.com/depot"
    dir: uploads

where:

1. `port` is port to listen on

2. `dbfile` is the path to `sqlite` file

3. `root` is the `approot` for the URL generation

4. `dir` is the directory where files are uploaded


Running
=======

    stack runghc -- -iapp app/Main.hs depconf.yaml

or:

    .stack-work/dist/x86_64-linux/Cabal-*/build/depot-exe/depot-exe depconf.yaml

