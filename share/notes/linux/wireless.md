# Additional info

Additional info is [here](https://metacpan.org/pod/network::troubleshoot-wireless)

# Firmware in Debian

For Latitude D630 I needed to install following package:

    sudo aptitude install firmware-iwlwifi

# Command line

## Restarting networking

    /etc/init.d/networking

## Configuring wireless

First of all, sometimes I had to enable WiFi from the command line, because turning on
the \`\`kill switch'' does not necessarily turns it on. It is done by the following command:

    sudo ifconfig wlan0 up

(where `wlan0` is whatever name is for the wireless interface)
Then we can scan. Notice that sometimes the results of scan are intermittent,
sometimes scan shows \`\`No scan results''. Anyway, here is how to scan:

    sudo iwlist eth1 scan
    sudo iwconfig eth1 essid "bestcoffee"
    sudo dhclient eth1

## WEP

Sometimes, people can give you 
the human-friendly version, and you can type that in like this:

    sudo iwconfig eth1 key s:password # translates to the hex for me.

Note the s: in front. That translates what I type into the hex jibberish.
Other times, people insist on giving you the goofy string of hex digits, so you can set it like this:

    sudo iwconfig eth1 key ACDB-1234-1234-EFG2

## WPA2

    sudo wpa_passphrase YOUR_SSID YOUR_WPA_KEY > /root/wpakey.conf
    sudo dhclient -r wlan0
    sudo wpa_supplicant -Dwext -iwlan0 -c/root/wpakey.conf
    #at this point, you may need to switch to an alternate terminal. (Ctrl+Alt+f2..f5).
    sleep 10
    sudo dhclient wlan0

See manual for wpa\_supplicant (in partucular, -Dwext is the device Driver, could be different)
However, there is a very long discussion here:

    http://ubuntuforums.org/showthread.php?t=571188

## Hidden networks

In ITEP I needed to connect to the hidden netword with essid `lab140`. The [WPA2](#wpa2) thing worked,
but for some reason in order for it to work I had to first execute:

    iwconfig wlan0 essid lab140

Maybe this was somehow related to the network being hidden.

## Removing all leases

Somehow they prevent from obtaining the new connection. Just remove this file:

    /var/lib/dhcp/dhclient.leases

# NetworkManager

Notice that when `/etc/init.d/network-manager` is running, the `wpa_supplicant` will not
work. Apparently, I have to choose between the `network-manager` and `wpa_supplicant`.

# MAC address

This is called \`\`HW'' in `ifconfig`, looks like it has the form `xx:xx:xx:xx:xx:xx`

# References

    http://blog.tplus1.com/index.php/2008/06/13/how-to-connect-to-a-wireless-network-from-the-ubuntu-command-line/
