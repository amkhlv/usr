Installation sequence on Debian
===============================

Notice that `davical` will also install `Apache2` and `PostgreSQL`
For the `PostgreSQL` to work we need to first configure `LOCALE`:

    dpkg-reconfigure locales

and set `LOCALE` to `en_US-UTF8`

    aptitude install davical

This also installs `Apache2` and `PostgreSQL`

Configure `PostgreSQL`
----------------------

Then do:

    vim /etc/postgresql/9.4/main/pg_hba.conf

and add two lines at the very top of that file:

    local   davical    davical_app   trust
    local   davical    davical_dba   trust

__After that__  automatically configure the `PostgreSQL`:

    su postgres -c /usr/share/davical/dba/create-database.sh


Configure `Apache2`
-------------------

The `VirtualHost` stanza (in `sites-available/...`) is like this:

    <IfModule mod_ssl.c>
      <VirtualHost _default_:443>
        ServerAdmin myname@gmail.com
        ErrorLog /var/log/apache2/servername.com/error.log
        CustomLog /var/log/apache2/servername.com/access.log combined

        DocumentRoot /usr/share/davical/htdocs
        Alias /robots.txt /var/www/servername.com/robots.txt

        DirectoryIndex index.php index.html
        Alias /images/ /usr/share/davical/htdocs/images/

        <Directory /usr/share/davical/htdocs/>
          AllowOverride None
          Require all granted
          php_admin_flag engine on
        </Directory>

        <IfModule mod_php5.c>
          php_value include_path /usr/share/awl/inc
          php_value magic_quotes_gpc 0
          php_value register_globals 0
          php_value error_reporting "E_ALL & ~E_NOTICE"
          php_value default_charset "utf-8"
        </IfModule>

        <IfModule mod_rewrite.c>
          RewriteEngine On
          # Not if it's the root URL.  You might want to comment this out if you
          # want to use an explicit /index.php for getting to the admin pages.
          RewriteCond %{REQUEST_URI} !^/$
          # Not if it explicitly specifies a .php program, stylesheet or image
          RewriteCond %{REQUEST_URI} !\.(php|css|js|png|gif|jpg)
          # Everything else gets rewritten to /caldav.php/...
          RewriteRule ^(.*)$ /caldav.php$1  [NC,L]
        </IfModule>

        SSLEngine on
        SSLCertificateFile /etc/apache2/ssl/apache.crt
        SSLCertificateKeyFile /etc/apache2/ssl/apache.key
      </VirtualHost>
    </IfModule>

Also, the `SSL` support has to be enabled:

    a2enmod ssl
    apache2ctl restart

Configure `DAViCal`
-------------------


Changing the password
=====================

The original password can be learned by doing:

    sudo -u postgres psql davical -c 'select username, password from usr;'

Notice that the original password is stored in __plaintext__ --- it is the expression __after two asterisks__

Then change the password by navigating to the Web interface, then logging in with that original password, and
changing the password in that web interface.

