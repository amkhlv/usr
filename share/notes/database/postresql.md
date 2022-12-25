PostgreSQL
==========

Installation
------------

    aptitude install postgresql

Setup
-----

### First access and setting up user

    su postgres
    psql

This will open the prompt: `postgres=# `

    CREATE ROLE andrei LOGIN;
    CREATE DATABASE andrei;
    ALTER USER andrei WITH ENCRYPTED PASSWORD 'somepassword';

Now user `andrei` can connect by executing:

    psql

### Configuration files and data directory

    SHOW data_directory;
    SHOW config_file;
    SHOW unix_socket_directories;

(this only works for admin, so need to `su postgres` first)

Remote connection
-----------------

For identification via certificate,
the line in `/etc/postgresql/../../pg_hba.conf` should be:

    hostssl andrei andrei 0.0.0.0/0 cert

The files `root.crt`, `server.crt` and `server.key` should go to `data_directory` (see [Configuration files and data directory])

The `config_file` should contain:


    ssl_cert_file = '/var/lib/postgresql/9.6/main/server.crt'               # (change requires restart)
    ssl_key_file = '/var/lib/postgresql/9.6/main/server.key'                # (change requires restart)
    ssl_ca_file = '/var/lib/postgresql/9.6/main/root.crt'                   # (change requires restart)


psql
----

### To show all databases

First, choose your database

    \c database_name

Then, this shows all tables in the current schema:

    \dt

### To show schemas

    \dn

