Building
========

First config:

    ./Config

When it asks, tell it to be install `/var/anope` ; then:

    cd build
    make
    make install

    chown -R irc:irc /var/anope

Configuring
===========

    cd /var/anope/conf
    cp example.conf services.conf

Edit the file `services.conf` as follows.

Name of services host
---------------------

Somewhere at the very top:

    define
    {
            name = "services.host"
            value = "services.andreimikhailov.com"
    }

Uplink
------

    uplink
    {

            host = "192.168.88.1"
            ipv6 = no
            ssl = no
            port = 6667
            password = "mypassword-1"
    }

Serverinfo
----------

    serverinfo
    {
            name = "services.andreimikhailov.com"
            description = "Services for the oceano IRC Networks"
            id = "69B"
            pid = "data/services.pid"
            motd = "conf/services.motd"
    }

Protocol module
---------------

    module
    {
            name = "hybrid"
            use_server_side_mlock = yes
            use_server_side_topiclock = yes
    }

Running and troubleshooting
===========================

To run, say:

    bin/services

More precisely:

    sudo -u irc bin/services

To dump diagnostics, say:

    sudo -u irc bin/services --nofork

This prints all kind of info messages on the terminal.
