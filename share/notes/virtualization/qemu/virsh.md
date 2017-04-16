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

By the way saying `virsh` opens a nice administrative shell with many useful commands.

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
