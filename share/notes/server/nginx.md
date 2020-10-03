
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

Each location can have its own separate password file.

The password file consists of the lines of the form:

    username:hash

Where hash is obtained by running the command:

    mkpasswd -m sha-512 -R 10000


# Running

## Setup the nginx password

See ["Preparing the password file"](#preparing-the-password-file) above.

## Starting and stopping

To start:

    /etc/init.d/nginx start

Other options are `stop`, `restart`, `reload`, ...

