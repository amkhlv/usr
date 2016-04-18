Install
=======

    aptitude install virt-manager

Run as root
===========

`virsh` and `virt-manager` are both run as root.

Permissions to set on files
===========================

    libvirt-qemu:libvirt-qemu

but `virt-manager` will automatically set such permission when the `qemu` is started (and sets it back to what it was after the virtual machine is turned off).

Creating an isolated network with libvirt
=========================================

From [redhat](https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Virtualization_Host_Configuration_and_Guest_Installation_Guide/App_Macvtap.html):

Add and save the following XML in the `/tmp/isolated.xml` file. If the `192.168.254.0/24` network is already in use elsewhere on your network, you can choose a different network.

    <network>
      <name>isolated</name>
      <ip address='192.168.254.1' netmask='255.255.255.0'>
        <dhcp>
          <range start='192.168.254.2' end='192.168.254.254' />
        </dhcp>
      </ip>
    </network>

Create the network with this command:

    virsh net-define /tmp/isolated.xml

Set the network to autostart with the `virsh net-autostart` isolated command.

Start the network with the:

    virsh net-start isolated

Using `virsh edit name_of_guest`, edit the configuration of each guest that uses macvtap for its network connection and add a new `<interface>` in the `<devices>`
section similar to the following (note the `<model type='virtio'/>` line is optional to include):

    <interface type='network'>
      <source network='isolated'/>
      <model type='virtio'/>
    </interface>

Shut down, then restart each of these guests. 


Setup video
===========


`virsh edit MACHINENAME` and include:

    <graphics type='spice' autoport='yes'>
        <streaming mode='off'/>
    </graphics>


Moving to different machine
===========================

as advised [here](http://serverfault.com/questions/434064/correct-way-to-move-kvm-vm) :

1. copy the VM's disks from on the src host to the same dir on the destination host

2. on the source host run `virsh dumpxml VMNAME > domxml.xml` and copy this xml to the destination host

3. on the destination host run `virsh define domxml.xml`
