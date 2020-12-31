# File server

## Database

The `depot.sqlite` should contain a `files` table:

    CREATE TABLE files (sha256 text, owner text, filename text, dt text, comment text);

## nginx

    location /depot/ {
        proxy_pass http://127.0.0.1:20000/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }

## systemd

    [Unit]
    Description=file depot
    
    [Service]
    WorkingDirectory=/var/www/depot-node
    ExecStart=/usr/bin/node index.js
    User=www-data
    
    [Install]
    WantedBy=multi-user.target

## config

The configuration file `config.yaml` should be like this:

    port: 20000,
    prefix: /depot
    sessionSecret: ...
    workingPath: /var/www/depot-node
    storagePath: /var/www/depot-node/files
    sqliteFile: depot.sqlite
    users: 
      - login: ...
        hash: ...
        salt: ...

The password hash is obtained by running:

    node amkhlv-hash-pwd.js SOMESALT

(it will ask for the password, which will then be hashed with `SALT`)
