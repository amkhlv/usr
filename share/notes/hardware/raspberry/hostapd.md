Install
=======

    aptitude install hostapd dnsmasq

Configure
=========

__REBOOT FREQUENTLY__ 

(for some reason, changes to `hostapd` configuration ofter do not work without reboot)

Hostapd
-------

Example `/etc/hostapd/hostapd.conf` :

    interface=wlan0
    # Name of the new network: best use the hostname
    ssid=somename
    ieee80211n=1

    # Pick a channel not already in use
    channel=2
    # Change to b for older devices?
    hw_mode=g
    macaddr_acl=0
    auth_algs=1
    # Disable this to insure the AP is visible:
    ignore_broadcast_ssid=0

    wpa=2
    wpa_passphrase=mypassphrase
    wpa_key_mgmt=WPA-PSK
    wpa_pairwise=TKIP
    rsn_pairwise=CCMP


Network address
---------------

configure in `/etc/systemd/network/10-wlan0.network` , _e.g_ :

    [Match]
    Name=wlan0

    [Network]
    Address=192.168.NN.1/24


DNSMasq
-------

`/etc/dnsmasq.conf` should contain lines:


    interface=wlan0
    dhcp-range=192.168.NN.2,192.168.NN.20,255.255.255.0,24h


