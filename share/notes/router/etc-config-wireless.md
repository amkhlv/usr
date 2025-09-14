iw list
=======

- Displays wireless capabilities of the radio (bands, supported channels, htmode, DFS, etc.).
- Useful to confirm what your hardware actually supports.
- Example:
    
    iw list | less
    
wifi command
============

    wifi status | jq

    wifi down

    wifi up



use /etc/rc.local
=================

- Script executed at the **end of boot sequence**.
- Custom commands placed here will run automatically on startup.
- Example (force Wi-Fi restart with delay):
    
    sleep 15
    /etc/init.d/network restart
    exit 0

Structure of /etc/config/wireless
=================================


This file defines Wi-Fi radios and Wi-Fi interfaces.

## config wifi-device

Represents a physical radio (phy0, phy1, …).

Options include:

    option type 'mac80211'      # driver framework
    option path 'platform/...'  # hardware bus path
    option band '2g' or '5g'
    option channel '6'          # fixed channel (avoid 'auto')
    option country 'BR'
    option htmode 'HT20' / 'VHT80' / etc.
    option disabled '0'         # 1 = disabled, 0 = enabled


## config wifi-iface

Represents a logical Wi-Fi interface on a radio (AP, client, mesh).

Options include:

    option device 'radio0'      # attach to which wifi-device
    option mode 'ap'            # ap = Access Point, sta = Client
    option ssid 'MyWiFi'
    option encryption 'psk2'    # WPA2
    option key 'SecretPass'
    option network 'lan'        # binds to a network in /etc/config/network


👉 Multiple wifi-iface sections can exist on the same wifi-device.
Example: one radio hosting both a main SSID and a guest SSID.

