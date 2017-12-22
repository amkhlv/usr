# DHCP problems

`OpenWRT` uses the lightweight DHCP client called `udhcpc` which is not very good. 
The problem comes when there is a parasitic DHCP server sending DHCP NAK. 

I managed to install the actual `dhclient` 

Then, to overcome the NAK problem, do the following:

    ifconfig br-wan down
    ifconfig br-wan up
    dhclient br-wan
