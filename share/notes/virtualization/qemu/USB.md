
USB passthrough
===============

First define the USB device in XML file
---------------------------------------

Suppose `lsusb` gives:

    Bus 001 Device 007: ID 03f0:0053 Hewlett-Packard

Then create a file 'hp.xml' with contents:

    <hostdev mode='subsystem' type='usb'>
         <source>
                 <vendor id='0x03f0'/>
                 <product id='0x0053'/>
         </source>
     </hostdev>

Then attach it
--------------

With `mymachine` __running__ :

    virsh attach-device   mymachine   hp.xml

