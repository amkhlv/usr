Example
=======

    config wifi-device 'radio0'
            option type 'mac80211'
            option channel '11'
            option hwmode '11g'
            option path 'platform/ar933x_wmac'
            option htmode 'HT20'
            option country 'US'

    config wifi-iface 'default_radio0'
            option device 'radio0'
            option network 'lan'
            option mode 'ap'
            option ssid 'myessid'
            option encryption 'psk2+ccmp'
            option key 'mycleartextpassphrase'
            option macfilter 'allow'
            list maclist 'aa:aa:aa:aa:aa:aa'
            list maclist 'bb:bb:bb:bb:bb:bb'
            list maclist 'cc:cc:cc:cc:cc:cc'


Apparently, it is [possible to be client (sta) and AP at the same time](https://wiki.openwrt.org/doc/recipes/ap_sta)

option mode
-----------

1. `ap` for Access Point,

2. `sta` for managed (client) mode,

3. `adhoc` for Ad-Hoc,

4. `wds` for static WDS, 

5. `monitor` for monitor mode,

6. `mesh` for IEEE 802.11s mesh mode

To restart
==========

Just type:

    wifi

and press ENTER...
