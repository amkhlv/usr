# General comments on OpenWRT

Every router needs its own image of OpenWRT. There is 
[oldstable](http://downloads.openwrt.org/backfire/),
[stable](http://downloads.openwrt.org/attitude_adjustment/)
and [snapshots](http://downloads.openwrt.org/snapshots/).

To clean up and start over:

    mtd -r erase rootfs_data

# Setup Checklist

## Flushing

Before flushing, **disconnect WAN**

## Initial setup

Immediately after flushing:

      telnet 192.168.1.1

and change password (`passwd`)

Then disconnect and connect via `ssh`

### Install and configure LuCI 

     opkg update
     opkg install luci

The file `/etc/config/uhttpd` should contain:

         list listen_http '192.168.1.1:80'
     #   list listen_http '[::]:80'
         list listen_https '192.168.1.1:443'
     #   list listen_https '[::]:443'

### Dropbear 

Authorized keys are in `/etc/dropbear/authorized_keys`

Listens on LAN only `vi /etc/config/dropbear`:

     config dropbear
            option PasswordAuth 'on'
            option RootPasswordAuth 'on'
            option Port         '22'
            option Interface    'lan'

## remaining setup via LuCI

### Change the machine name

     System -> System

and also timezone

### Override MAC address for WAN with something random

     Network -> Interfaces -> WAN 

Then 

     cat /dev/random > eraseme.txt

Then `gvim eraseme.txt` and in `gvim`:

     :%!xxd


### Setup wireless

In Advanced Settings, set country

Wireless Security

Choose \`\`WPA2-PSK\`\`, then \`\`Force CCMP (AES)'' --- based on reading, perhaps this gives most security...
[A nice writeup](https://learningnetwork.cisco.com/thread/11207).

MAC Filter: Choose __Allow listed only__. On Android, the MAC address can be found in:

# Passwordless login

The public keys should be installed as follows:

    ssh-keygen -t dsa
    cat ~/.ssh/id_dsa.pub | ssh -l root 192.168.1.1 -p 29649 " cat >> /etc/dropbear/authorized_keys "

# Image Generator

For `Attitude Adjustment`, download the `Image Generator` from [here](http://downloads.openwrt.org/attitude_adjustment/12.09/ar71xx/generic/OpenWrt-ImageBuilder-ar71xx_generic-for-linux-i486.tar.bz2) --- notice that this is **more than 350M** !

Then `tar -xvf`, go there and execute the command:

    make image PROFILE=TLWR740 PACKAGES=... FILES=...

Specific details are explained [here](http://wiki.openwrt.org/doc/howto/obtain.firmware.generate). 

Example: `PACKAGES="nano openvpn -ppp -ppp-mod-pppoe"` will build with included `nano` and `openvpn`, but excluded `ppp` and `ppp-mod-pppoe`

A directory with custom files to add can be specified using the `FILES` variable. Custom files will replace default ones if necessary.
For example, the command below will push some of the configuration files into the squash:

     mkdir -p files/etc/config 
     scp root@192.168.1.1:/etc/config/network files/etc/config/ 
     scp root@192.168.1.1:/etc/config/wireless files/etc/config/ 
     scp root@192.168.1.1:/etc/config/firewall files/etc/config/ 
     make image PROFILE=WL500GP PACKAGES="nano openvpn -ppp -ppp-mod-pppoe" FILES=files/

The **resulting images** are in `bin/ar71xx/`

To **clean up** temporary build files and generated images, use the `make clean` command. 

# Dynamic DNS

## Setting up Curl with SSL

We will set up Dynamic DNS with `curl` over `SSL`.
The command for Image Builder is:

    make image PROFILE=TLWR740 PACKAGES="-kmod-ledtrig-usbdev -kmod-usb-core -kmod-usb-ohci -kmod-usb2 luci curl"

This leaves about 576K on the overlay.

> &#x3d;item\* Install `ca-certificates.crt` on the router.
>
> On the router: `mkdir /etc/ssl/certs`
>
> On the main computer:
>
>     scp -P 29649 ca-certificates.crt root@192.168.1.1:/etc/ssl/certs/
>
> Notice that this is 240K ! **Open question**: is it possible to use something smaller?
>
> &#x3d;item\* Install `ddns-scripts`:
>
>     opkg install ddns-scripts
>
> &#x3d;item\* Configuring ddns
>
> The `LuCI` interface is not entirely adequate for the SSL setup, so we have to edit the configuration files.
>
> The content of `/etc/config/ddns`:
>
> <div>
>     <div style="color:red;"> <b>Note:</b> do not change "myddns"</div>  in the first line (the service name); obviously it is referred somewhere else, dont know where...
> </div>
>
> &#x3d;item\* Also modify `/usr/lib/ddns/services`:
>
> (Only different from the defaults by the letter "s" in "https")
>
> (Notice that this doubles the `update_url` section in `/etc/config/ddns`, I dont know why this is needed but otherwise did not work!)
>
> &#x3d;item\* Graphical interface via LuCI
>
>     opkg install luci-app-ddns
>
> &#x3d;item\* Debugging
>
> If something goes wrong, you can see what is happenning by calling
>
>     /usr/lib/ddns/dynamic_dns_updater.sh myddns
>
> Note: myddns is the name of the service config entry in /etc/config/ddns file. 


# DHCP problems

`OpenWRT` uses the lightweight DHCP client called `udhcpc` which is not very good. 
The problem comes when there is a parasitic DHCP server sending DHCP NAK. 

I managed to install the actual `dhclient` 

Then, to overcome the NAK problem, do the following:

    ifconfig br-wan down
    ifconfig br-wan up
    dhclient br-wan
