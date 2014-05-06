# Setting up for resque

    mount /dev/mapper/sdb2_crypt chr
    mount -t proc none chr/proc
    mount -o bind /dev chr/dev
    mount -t sysfs sys chr/sys



# Doing chroot

    chroot chr /bin/bash

Then, if I want the network, I have to edit `/etc/resolv.conf`:

    domain lan
    search lan
    nameserver 192.168.1.1
