# Password-based authentication, CSRF protection

## nginx

    location /localsite/ {
        proxy_pass http://127.0.0.1:20000/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }

## systemd

    [Unit]
    Description = Local Site
    After = network.target
    
    [Service]
    PIDFile = /run/localsite.pid
    WorkingDirectory = /path/to/Node/localsite/
    User = andrei
    ExecStart = /path/to/Node/localsite/start.sh
    
    [Install]
    WantedBy = multi-user.target

## config

The configuration file `config.yaml` should be to the scheme:

    {
        port: int,
        prefix: string,
        sessionSecret: string,
        workingPath: string,
        sqliteFile: string,
        users: {
            login: string,
            hash: string,
            salt: string
        }
    }

The password hash is obtained by running:

    node amkhlv-hash-pwd.js SOMESALT

(it will ask for the password, which will then be hashed with `SALT`)
