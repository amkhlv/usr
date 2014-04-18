# Configuration with SSL

## Preparing the self-signed certificate

We will store the certificate in `/etc/nginx/`:

    cd /etc/nginx
    openssl req -new -x509 -days 365 -nodes -out amk.pem -keyout amk.key

## NGINX site configuration

The site configuration files are in `/etc/nginx/sites-available/` which are then symlinked to `/etc/nginx/sites-enabled/`

### Preamble of the server conf file

Taking into account recommendations from [wiki.mozilla.org](https://wiki.mozilla.org/Security/Server_Side_TLS#Nginx)

    server {
        listen 443;
        if ($http_transfer_encoding ~* chunked) {
            return 444;
        }

        server_name localhost
                    yourservername.com
                    ;
        ssl  on;
        ssl_certificate  amk.pem;
        ssl_certificate_key  amk.key;

        ssl_session_timeout  5m;

        ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:ECDHE-RSA-RC4-SHA:ECDHE-ECDSA-RC4-SHA:AES128:AES256:RC4-SHA:HIGH:!aNULL:!eNULL:!EXPORT:!DES:!3DES:!MD5:!PSK;
        ssl_protocols SSLv3 TLSv1 TLSv1.1 TLSv1.2;
        ssl_prefer_server_ciphers   on;

        access_log /var/log/nginx/secureserver.access.log;
        error_log /var/log/nginx/secureserver.error.log;

        client_max_body_size       30M;

    #### HERE below go LOCATIONS,

    }

### Locations

Locations are described like this:

    location /hidden/ { 
        auth_basic "Restricted";
        auth_basic_user_file htpasswd_for_hidden;
        alias /var/www/hidden/;
        autoindex on;
        expires 30d;
    }

I will need:

    mkdir /var/www/hidden
    chown www-data:www-data /var/www/hidden

## Preparing the password file

Each location can have its own separate password file, which is obtained by running my script:

    amkhlv-hash.sh andrei > htpasswd_for_hidden

The content of the script `amk-hash.sh`:

    #!/bin/bash

    [ "$1" ] || { echo 'ERROR: $1 should be username'; exit 1 ; }

    printf "$1:$(openssl passwd -1 )\n"

<div>
    <div style="color:red;"><b> Do not use that Perl module crypt("password", "salt")</b></div> because it is highly insecure, for example it cuts the password to the first 8 characters !
</div>

# Running

## Setup the nginx password

See ["Preparing the password file"](#preparing-the-password-file) above.

## Starting and stopping

To start:

    /etc/init.d/nginx start

Other options are `stop`, `restart`, `reload`, ...

