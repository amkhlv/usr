Building
========

Follow [instructions here](http://www.yesodweb.com/page/quickstart)

Deployment
==========

The directory `/var/www/linii` should be present on the server; it is populated by executing `deploy.sh`

NGINX
-----

    location /linii/ {
        auth_basic "Enter password for linii";
        auth_basic_user_file htpasswd_linii;
        rewrite /linii(.*) $1  break;
        proxy_pass http://127.0.0.1:11111;
    }


Systemd
-------

    [Unit]
    Description=linii

    [Service]
    WorkingDirectory=/var/www/linii
    Environment="PORT=11111"
    Environment="HOST=127.0.0.1"
    Environment="APPROOT=https://mysite.com/linii"
    ExecStart=/usr/local/bin/linii
    User=www-data

    [Install]
    WantedBy=multi-user.target


Python
------

    aptitude install python-vobject

