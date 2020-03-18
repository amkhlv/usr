# Bookmarks server

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

The configuration file `~/.config/amkhlv/localsite/config.yaml` should be to the scheme:

    {
        port: int,
        sessionSecret: string,
        workingPath: string,
        staticPath: string,
        musicFile: string,
        calendarFile: string,
        sqliteFile: string,
        prefix: string,
        users: {
            login: string,
            hash: string,
            salt: string
        }
    }
