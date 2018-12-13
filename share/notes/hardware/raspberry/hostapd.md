An excellent writeup is [here](http://hawksites.newpaltz.edu/myerse/2018/06/08/hostapd-on-raspberry-pi/)

And NAT config:

    *nat
    -A POSTROUTING -o eth0 -j MASQUERADE

    *filter
    -A FORWARD -i eth0 -o wlan0 -m state --state RELATED,ESTABLISHED -j ACCEPT
    -A FORWARD -i wlan0 -o eth0 -j ACCEPT

