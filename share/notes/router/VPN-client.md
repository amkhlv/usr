Setting up VPN client
=====================

Install packages
----------------

    opkg install openvpn-openssl luci-app-openvpn 

### Manual config

 to configure manually. The file `/etc/config/openvpn` should contain lines:

    config openvpn 'my_client'
            option config '/etc/openvpn/my.ovpn'
            option enabled '1'

It seems that the name should be of the form `SOMEWORD_client`, like that, with underscore, and the last word is `client`

### LuCI config

It is in LuCI under `Services` tab.

It seems that the name should be of the form `SOMEWORD_client`, like that, with underscore, and the last word is `client`


Firewall config
---------------

Starting from `Openwrt 21...` there are some changes.
The tunnel interface does not show any more in `LuCI`'s `Interfaces`.
To add it to `LAN` zone, go to `Network` → `Firewall`, there edit the `lan` zone, in the `Advanced settings` tab, add it to `Covered devices`.
(The `covered devices` list is typically empty; the existing wired interfaces are somehow configured differently ???)

