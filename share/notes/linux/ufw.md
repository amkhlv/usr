Basic commands
==============

    ufw allow in on eth0 to any port 80 proto tcp
    ufw allow proto tcp from 172.30.15.10/32 to 172.30.15.16 port 80
    ufw allow 6667:7000

Observations:

1. `on` specifies the interface, _e.g._ `on eth0`

2. `from` and `to` specify the IP address

