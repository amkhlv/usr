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

IPtables
========

I want to be able to `ssh` to a virtual mahine from some container `Cont`; I need roughly this:

    /sbin/iptables -I FORWARD   -m set --match-set amkhlv-cont dst,dst   -m set --match-set amkhlv-qemu src,src   -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    /sbin/iptables -I FORWARD   -i ve-Cont   -m set --match-set amkhlv-qemu dst,dst   -j ACCEPT
    /sbin/iptables -t nat -I POSTROUTING -o virbr1 -j MASQUERADE

(assuming that `isolated` network's bridge is `virbr1`).

The __problem__ is, that `qemu` inserts new rules into `iptables` every time the virtual machine is started. Therefore, I should do my insertions __every time after__
the virtual machine is started, and the corresponding `-D` after every virtual machine shutdown. This can be done by writing a hook `/etc/libvirt/hooks/qemu`
[along these lines](https://wiki.libvirt.org/page/Networking#Forwarding_Incoming_Connections)


