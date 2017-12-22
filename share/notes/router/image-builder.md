
Building image
==============

    make image  PROFILE=TLWR740  PACKAGES=...  FILES=...

Example: `PACKAGES="nano openvpn -ppp -ppp-mod-pppoe"` will build with included `nano` and `openvpn`, but excluded `ppp` and `ppp-mod-pppoe`

List of all existing profiles:

    make info

The resulting images are in `bin/ar71xx/`

To __clean up__ temporary build files and generated images, use the `make clean` command. 

Including files or replacing files
==================================

A directory with custom files to add can be specified using the `FILES` variable. Custom files will replace default ones if necessary.
For example, the command below will push some of the configuration files into the squash:

     mkdir -p files/etc/config 
     scp root@192.168.1.1:/etc/config/network files/etc/config/ 
     scp root@192.168.1.1:/etc/config/wireless files/etc/config/ 
     scp root@192.168.1.1:/etc/config/firewall files/etc/config/ 
     make image PROFILE=WL500GP PACKAGES="nano openvpn -ppp -ppp-mod-pppoe" FILES=files/




