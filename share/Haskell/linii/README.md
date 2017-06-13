Modifying
=========

This is based on `yesod-sqlite` template.

Because of [recent changes](http://www.yesodweb.com/blog/2017/06/updated-yesod-scaffolding):

__Do not__ edit `linii.cabal` , as it is auto-generated from `package.yaml`


Building
========

Use the `--fast` flag:

    stack build --fast

otherwise compilation could be very slow and memory-consuming.

Follow [instructions here](http://www.yesodweb.com/page/quickstart)


Configuration
=============

The IP and PORT to listen on, as well as the location of `sqlite` file, are configured
in `config/settings.yml`


Deployment
==========

The directory `/var/www/linii` should be present on the server; it is populated by executing `deploy.sh`

Inside `/var/www/linii` there is should be a configuration file:

    /var/www/linii/config/settings.yml

This has the same format as [the included sample config file](config/settings.yml).

Important lines are (assuming that PORT is 11111):

    host:           "_env:HOST:127.0.0.1" 
    port:           "_env:PORT:11111" 
    approot:        "_env:APPROOT:https://andreimikhailov.com/linii"



NGINX
-----

In `nginx.conf`:

    http {
        ...
        limit_req_zone $binary_remote_addr zone=antisurf:10m rate=10r/s;
        ...
    }

Then in locations file:

    location /linii/ {
        auth_basic "Enter password for linii";
        auth_basic_user_file htpasswd_linii;
        limit_req zone=antisurf burst=50;
        rewrite /linii(.*) $1  break;
        proxy_pass http://127.0.0.1:11111;
    }


Systemd
-------

    [Unit]
    Description=linii

    [Service]
    WorkingDirectory=/var/www/linii
    ExecStart=/usr/local/bin/linii config/settings.yml
    User=www-data

    [Install]
    WantedBy=multi-user.target


Python
------


    aptitude install python-vobject

