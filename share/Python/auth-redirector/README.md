Deploy
======

First, on the server:

    aptitude install python3-venv

Then as an ordinary user:

    python3 -mvenv auth-redirector

Then, on this machine:

    ./deploy.sh myserver:path/to/auth-redirector

Setup
=====

    ./setup.sh

Systemd unit
============

    [Unit]
    Description = AuthRedirector
    After = network.target

    [Service]
    PermissionsStartOnly = true
    PIDFile = /run/redirector.pid
    WorkingDirectory = /home/andrei/usr/share/Python/auth-redirector/redirector/
    Environment = SECRET_KEY='...'
    Environment = SOCIAL_AUTH_GOOGLE_OAUTH2_KEY='...'
    Environment = SOCIAL_AUTH_GOOGLE_OAUTH2_SECRET='...'
    Environment = SERVERNAME=MYHOSTNAME
    Environment = AROOT=depot
    ExecStart = /home/andrei/usr/share/Python/auth-redirector/bin/gunicorn --user=andrei --group=andrei -b localhost:8000 --pid /run/redirector.pid redirector.wsgi
    ExecReload = /bin/kill -s HUP $MAINPID
    ExecStop = /bin/kill -s TERM $MAINPID
    PrivateTmp = true

    [Install]
    WantedBy = multi-user.target

NGINX
=====

    upstream red {
            server 127.0.0.1:8000;
    }

    server {

    ...

    location /depot/ {
            proxy_set_header X-Forwarded-Host "MYHOSTNAME";
            proxy_set_header Host $host;
            proxy_pass http://red;
            proxy_redirect off;
    }
    location /redirector/USER1/ {
            internal;
            add_header Content-Type $upstream_http_content_type;
            alias /var/www/redirector/USER1/;
    }
    location /redirector/USER2/ {
            internal;
            add_header Content-Type $upstream_http_content_type;
            alias /var/www/redirector/USER2/;
    }

    }
