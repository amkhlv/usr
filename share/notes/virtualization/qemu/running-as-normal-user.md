How to connect to libvirt as a non-root user
============================================

This will allow connecting to `qemu:///system` as a normal (non-root) user. 

Taken [here](https://blog.night-shade.org.uk/2015/02/allow-virsh-as-a-normal-user-on-debian-jessie/) :

Configuring group
-----------------

    adduser andrei libvirt

Polkit
------

Create the file `/etc/polkit-1/localauthority/50-local.d/50-libvirt-virsh-access.pkla` with the following content:

    [libvirt Management Access]
    Identity=unix-group:libvirt
    Action=org.libvirt.unix.manage
    ResultAny=yes
    ResultInactive=yes
    ResultActive=yes

Environment variable
--------------------

    LIBVIRT_DEFAULT_URI=qemu:///system
