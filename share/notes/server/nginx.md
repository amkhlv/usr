
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

    aptitude install apache2-utils

Then, to create the new password file:

    htpasswd -c  htpasswd_for_hidden  username

(this will ask to enter new password for `username`)

To add new user:password :

    htpasswd  htpasswd_for_hidden  newusername

# Running

## Setup the nginx password

See ["Preparing the password file"](#preparing-the-password-file) above.

## Starting and stopping

To start:

    /etc/init.d/nginx start

Other options are `stop`, `restart`, `reload`, ...

