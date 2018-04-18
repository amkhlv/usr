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



Virsh shell
===========

By the way saying `virsh` opens a nice administrative shell with many useful commands, in particular:

     start MACHINENAME
     net-start isolated

Start viewer from the command line 
==================================

See [writeup on virt-viewer](virt-viewer.md)

Setup video
===========

`virsh edit MACHINENAME` and include:

    <graphics type='spice' autoport='yes'>
        <streaming mode='off'/>
    </graphics>


Moving to different machine
===========================

as advised [here](http://serverfault.com/questions/434064/correct-way-to-move-kvm-vm) :

1. Copy the VM's disks from on the src host to the same dir on the destination host

2. On the source host run `virsh dumpxml VMNAME > domxml.xml` and copy this xml to the destination host. Also
have to do this with networks: `virsh net-dumpxml NETWORKNAME > networkname.xml`.

3. On the destination host run `virsh define domxml.xml` and `virsh net-define networkname.xml`
