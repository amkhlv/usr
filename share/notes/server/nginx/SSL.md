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

