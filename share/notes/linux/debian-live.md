# Links

[The Manual for Stable](http://live.debian.net/manual/stable/html/live-manual.en.html)

# Installing and Configuration

    aptitude install live-build

    mkdir live-default
    cd live-default

# Building

Then either:

    lb init

or:

    lb config

depending on the version of `live-build`; there is some confusion about this. Actually `lb config` has tons of arguments:

    lb config --distribution stretch --archive-areas "main contrib non-free" --bootappend-live "boot=live config username=andrei" --mirror-binary-security http://security.debian.org/   --mirror-chroot-security http://security.debian.org/  --security true

Then:

    lb bootstrap
    lb chroot

First execute, just to set users and passwords:

    systemd-nspawn  -D chroot/

Then just boot it and install all that is needed:

    systemd-nspawn  -D chroot/  --network-macvlan=eth0  --boot

Comments:

1. Remember that to exit need to press `Ctrl-]` three times rapidly

2. the flat `--network-macvlan` puts a second `MAC` address on the same physical card, which then goes to the container; its name inside the container start with `mv-`; we have to remember that it should be configured (execute `dhclient` inside the container)

Finally make the ISO image:

    lb binary


# Cleanup

If something needs correction, clean up the images:

    lb clean --binary

correct the `chroot` filesystem, then `lb binary` again

# Useful programs

    apt-get install  aptitude stow git screen btrfs-tools systemd-container ntfs-3g lshw dmidecode socat cryptsetup

    apt-get install  xbindkeys gmrun xfonts-terminus emacs emacs-goodies-el shutter xfce4 xfce4-goodies xautomation xsel python3-markdown vim-gtk racket xul-ext-noscript

# Problems

## Several kernels

`lb binary` fails if the directory `chroot/boot/` has more than one linux kernel. Have to purge all older kernels.
