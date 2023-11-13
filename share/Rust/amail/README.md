

Setup PostgreSQL for mail indexing
==================================

First, as user `postgres`:

    CREATE ROLE amkhlv WITH SUPERUSER NOINHERIT;
    GRANT amkhlv to andrei;
    CREATE DATABASE emails;
    \c emails

Then, as normal user:

    SET ROLE amkhlv;
    \i psql-setup.sql

The ownership of the tables can then be given back to `andrei`:

    ALTER TABLE settings OWNER TO andrei;
    ALTER TABLE maildirs OWNER TO andrei;
    ALTER TABLE mail OWNER TO andrei;

The table `setting` should contain just one row, with `key` equals `default_search_folder` and
`value` equals the path to the maildir of the default search folder.
