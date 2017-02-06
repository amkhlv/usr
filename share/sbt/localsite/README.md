Installation
============

The `geckodriver` should be downloaded from [here](https://github.com/mozilla/geckodriver/releases) and copied to the location path
which is configured by the system parameter `-Dwebdriver.gecko.driver=...`


Configuration
=============

via `application.conf`. In particular:

Filters
-------

all __filter configuration parameters__ are [described here in detail](https://www.playframework.com/documentation/2.5.x/resources/confs/filters-helpers/reference.conf)


Configure GnuPG
---------------

The folder `~/.gnupg` should contain the file `gpg-agent.conf` with single line:

    allow-loopback-pinentry

Also, the file `gpg.conf` should have a line:

    pinentry-mode loopback


Running
=======

Systemd
-------

    [Service]
    Environment=XAUTHORITY=/home/andrei/.Xauthority
    Environment=DISPLAY=localhost:10.0
    ExecStart=/home/andrei/usr/share/sbt/localsite/target/universal/stage/bin/localsite -Dconfig.file=/home/andrei/.config/amkhlv/localsite.conf -Dhttp.port=disabled -Dhttps.port=%i -Dhttp.address=127.0.0.1 -Dpidfile.path=/tmp/amkhlv-localsite.pid -Dwebdriver.gecko.driver=/usr/local/lib/geckodriver
    PIDFile=/tmp/amkhlv-localsite.pid

