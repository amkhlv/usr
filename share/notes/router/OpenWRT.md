
# Flushing

Before flushing, **disconnect WAN**

## Flushing from the command line

     cat openwrt-ar71xx-generic-tl-wr740n-v5-squashfs-factory.bin | ssh 192.168.1.1 "mtd -r write - firmware"


# Initial setup

Immediately after flushing:

      telnet 192.168.1.1

**or sometimes**:

      ssh 192.168.1.1

and change password (`passwd`)

Then disconnect and connect via `ssh`

## Install and configure LuCI 

     opkg update
     opkg install luci

To configure `LuCI` via `LuCI`, must install:

    opkg install luci-app-uhttpd

Then, in web interface, appears tab `services` → `uhttpd` where listening interface can be configured.

Perhaps, __disable listening on WAN__.
However, the only way of doing it seems to be restricting the list of web addresses where `uhttpd` listens.
(And then, it should be changed each time when changing the LAN address.)

## Dropbear 

Authorized keys are in `/etc/dropbear/authorized_keys`

Listens on LAN only `vi /etc/config/dropbear`:

     config dropbear
            option PasswordAuth 'on'
            option RootPasswordAuth 'on'
            option Port         '22'
            option Interface    'lan'

## Hostname

In `/etc/config/system` , section `config system` :

    hostname mybox

## Override MAC address for WAN with something random

In `/etc/config/network` , sections `config interface 'wan'` and `config interface 'lan'`

     option macaddr 'xx:xx:xx:xx:xx:xx'

__do not__ override the first three bytes!

# Wireless

## Wireless Security

Choose `WPA2-PSK`, then `Force CCMP (AES)` --- based on
[some reading](https://learningnetwork.cisco.com/thread/11207)  
perhaps most secure...

MAC Filter: Choose __Allow listed only__. 

## Setting up via conf files

See [example](etc-config-wireless.md)

