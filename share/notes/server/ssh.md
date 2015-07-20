Server
======

Installation
------------

    aptitude install openssh-server

The main configuration files are in the directory /etc/ssh :

    ssh_config : client configuration file

    sshd_config : server configuration file


Configuration
-------------

    vim /etc/ssh/sshd_config

and pay attention to:

    AllowUsers andrei
    
