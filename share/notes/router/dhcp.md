Assign static DHCP to a device
==============================

In `/etc/config/hdcp` :

    config 'host'
        option 'name' 'myphone'
        option 'ip'   '192.168.1.100'
        option 'mac'  '00:11:22:33:44:55'
