Links
=====

[Ubuntu Quick](http://moinmo.in/HowTo/UbuntuQuick#Nginx_Installation)

[ERP5](http://www.erp5.org/MoinMoin/InstallDocs)

Installation and configuration
==============================

Installation
------------

    aptitude install python-moinmoin

Configuration
-------------

_Let us call it:_ __amqwiki__

### Copying files and setting permissions

    mkdir -p /var/www/amqwiki
    mkdir -p /var/lib/amqwiki
    cp -r /usr/share/moin/data /usr/share/moin/underlay /var/lib/amqwiki
    chown -R www-data:www-data /var/lib/amqwiki /var/www/amqwiki
    chmod -R ug+rwX /var/lib/amqwiki/
    chmod -R o-rwx /var/lib/amqwiki/

_Comment:_ Neat trick permission - `X`. There's a special permission option you can send to chmod: a capital `X`.
X refers to execute permission just for directories and for files that already have execute permission for one or more categories.

### Configuration files:

1. `/etc/moin/farmconfig.py` should contain the following lines:

         wikis = [
             ("amqwiki", r".*"),   # this is ok for a single wiki
         ]

2. `/etc/moin/mywiki.py` --- this should be copied to `amqwiki.py` ; see [contents below](#amqwiki), but roughly speaking:
    - Change sitename and wikiname to your wiki's name
    - Change datadir to data_dir = '/var/lib/amqwiki/data'
    - Change data underlay dir to data_underlay_dir = '/var/lib/amqwiki/underlay'

Running as standalone server
============================

Startup command
---------------

    moin --config-dir=/etc/moin server standalone --user=www-data --group=www-data --port=19111 --docs=/usr/share/moin/htdocs

Nginx location
--------------

        location ~ (/amq_?/|/FrontPage|/HelpContents|/FindPage|/RecentChanges) {
                try_files $uri @wiki;
        }
        location @wiki {
                rewrite ^/amq_/(.*) /$1 break;
                auth_basic "NoteBook";
                auth_basic_user_file htpasswd_nb;
                proxy_pass http://127.0.0.1:19111;
        }

--- this is ugly, one of the reasons why better to [run under Apache](#sec:Apache)

Conf
----

It looks like all settings are in `/etc/moin/amqwiki.py` 

### Contents of `/etc/moin/amqwiki.py` 

<a name="amqwiki"></a>

So, I __add__ the following lines to `/etc/moin/amqwiki.py` :

    data_dir = '/var/lib/amqwiki/data'
    data_underlay_dir = '/var/lib/amqwiki/underlay'
    superuser = [u"andrei", ]
    acl_rights_before = u"andrei:read,write,delete,revert,admin"
    url_prefix_static = '/amq_/moin_static198' 

Unfortunately, it seems like `moin_static198` is hard-coded into `MoinMoin` (it follows the version number `1.9.8`).
Each time `moinmoin` gets updated I have to return to this configuration file. It seem that there are no other
configuration files where this should be entered.

Running under Apache
====================

<a name="sec:Apache"></a>

Installing Apache
-----------------

    aptitude install apache2 apache2-utils libapache2-mod-wsgi

MoinMoin configuration and WSGI script
--------------------------------------

The configuration still happens in `/etc/moin/amqwiki.py` ! (I guess this is specified in `/etc/moin/farmconfig.py`)

The content of this file is the same as [it was in for the standalone conf](#amqwiki).

We will just continue from the previous installation:

    cp /usr/share/moin/server/moin.wsgi /var/lib/amqwiki/
    chown -R www-data:www-data /var/lib/amqwiki/moin.wsgi
    chmod -R ug+rwx /var/lib/amqwiki/moin.wsgi
    chmod -R o-rwx /var/lib/amqwiki/moin.wsgi

Apache configurations
---------------------

As we proxy everything through `nginx`, we only need `Apache` listening on a local port.

### Edit `ports.conf` :

    Listen 127.0.0.1:19080

    <IfModule ssl_module>
            Listen 127.0.0.1:19443
    </IfModule>

    <IfModule mod_gnutls.c>
            Listen 127.0.0.1:19443
    </IfModule>

### Edit `/etc/apache2/apache2.conf`

and add at the end:

    #  MoinMoin WSGI configuration
    #
    # you will invoke your moin wiki at the root url, like http://servername/FrontPage:
    WSGIScriptAlias /amq_    /var/lib/amqwiki/moin.wsgi

    # create some wsgi daemons - use these parameters for a simple setup
    WSGIDaemonProcess moin user=www-data group=www-data processes=5 threads=10 maximum-requests=1000 umask=0007

    # use the daemons we defined above to process requests!
    WSGIProcessGroup moin

    <Directory /var/lib/amqwiki/>
      Require all granted
      Options +ExecCGI
    </Directory>

    <Files /var/lib/amqwiki/moin.wsgi>
      Require all granted
      Options +ExecCGI
    </Files>

    <Directory "/usr/share/moin/htdocs/">
      AllowOverride All
      Require all granted
      Options +ExecCGI
    </Directory>

Notice that such syntax will work on `Apache 2.4`, but for earlier versions of `Apache` one has to use `Allow from all`
instead of `Require all granted`. Some people do this:

      <IfVersion < 2.3 >
       Order allow,deny
       Allow from all
      </IfVersion>
      <IfVersion >= 2.3>
       Require all granted
      </IfVersion>

Also notice that we set the URL prefix `/amq` (we were not able to do it using the standalone server).
This simplifies the location configuration of `nginx`.

Nginx
-----

      location /amq_/ {
              try_files $uri @wiki;
      }
      location @wiki {
              auth_basic "NoteBook";
              auth_basic_user_file htpasswd_nb;
              proxy_pass http://127.0.0.1:19080;
      }
