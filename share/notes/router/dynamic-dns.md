
We will set up Dynamic DNS with `curl` over `SSL`.

Setting up Curl with SSL
========================

First install `curl`:

    opkg update
    opkg install curl


Installing  ca-certificates 
---------------------------

On the router: 

    mkdir /etc/ssl/certs

On the main computer:

    scp -P 29649 ca-certificates.crt root@192.168.1.1:/etc/ssl/certs/

Notice that this is 240K ! 

Install and configure ddns
==========================

    opkg install ddns-scripts

Edit `/etc/config/ddns`.

<div style="color:red;"> <b>Note:</b> do not change "myddns"</div>  in the first line (the service name)

Modify /usr/lib/ddns/services
-----------------------------

(Only different from the defaults by the letter "s" in "https")

(Notice that this doubles the `update_url` section in `/etc/config/ddns`, I dont know why this is needed but otherwise did not work!)

Graphical interface via LuCI
============================

    opkg install luci-app-ddns

Debugging
=========

If something goes wrong, we can see what is happenning by calling

    /usr/lib/ddns/dynamic_dns_updater.sh myddns

Note: myddns is the name of the service config entry in `/etc/config/ddns` file. 

