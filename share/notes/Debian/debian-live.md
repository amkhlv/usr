# Links

[The Manual for Stable](http://live.debian.net/manual/stable/html/live-manual.en.html)

# Installing and Configuration

    aptitude install live-build

    mkdir live-default
    cd live-default

# Building

## Configuring and debootstrapping

Then either:

    lb init

or:

    lb config

depending on the version of `live-build`; there is some confusion about this. Actually `lb config` has tons of arguments:

    lb config --distribution stretch --archive-areas "main contrib non-free" --bootappend-live "boot=live config username=andrei" --mirror-binary-security http://security.debian.org/   --mirror-chroot-security http://security.debian.org/  --security true

by default, `live-build` __does not include Debian Installer images__ in the images. 
It needs to be specifically enabled with lb config. Also, please note that for the "Desktop" installer to work, 
the kernel of the live system must match the kernel d-i uses for the specified architecture. For example:

    lb config --architecture amd64 --debian-installer live --packages debian-installer-launcher

Then:

    lb bootstrap
    lb chroot

## Running in container, adjustments

First execute, just to set users and passwords:

    systemd-nspawn  -D chroot/

Then just boot it and install all that is needed:

    systemd-nspawn  -D chroot/  --network-macvlan=eth0  --boot

Comments:

1. Remember that to exit need to press `Ctrl-]` three times rapidly

2. the flat `--network-macvlan` puts a second `MAC` address on the same physical card, which then goes to the container; its name inside the container start with `mv-`; we have to remember that it should be configured (execute `dhclient` inside the container)

## Binary stage

Finally, __to make the ISO image__:

    lb binary

The command `lb binary` actually consists of various lower level commands which are explained by typing:

    live-build --usage

The first one, `lb binary_chroot`, "duplicates  the  chroot  directory". This means that everything in `chroot/` is first moved into `chroot/chroot/`, and
then `chroot` is somehow rebuilt... When `lb binary` exits cleanly, this is reverted back to original. However, if `lb binary` does not exit, then
the `chroot` gets messed up...

# Cleanup and rebuild

## Comment on chroot.packages.install and chroot.packages.live and chroot.files

I suspect that I __need to update them for every rebuild__. 
(If I dont, the `apt-get` goes through the upgrade during rebuild... (?) )

They should be in the root of the `live-build` folder.
In my situation, these two files are identical. They are obtained by:

    systemd-nspawn -D chroot
    cd /
    dpkg-query -W > chroot.packages.install
    dpkg-query -W > chroot.packages.live
    ls -lR > chroot.files

and then moving the resulting files into the root of the `live-build` folder. In `chroot.files`, obviously, have to remove the line listing the `chroot.files` itself :)

## Rebuilding

    lb clean --binary
    rm -rf cache/ 

correct the `chroot` filesystem, then again: 

    lb binary


# Useful programs

    apt-get install  aptitude stow git screen btrfs-tools systemd-container ntfs-3g lshw dmidecode socat cryptsetup

    apt-get install  xbindkeys gmrun xfonts-terminus emacs emacs-goodies-el shutter xfce4 xfce4-goodies xautomation xsel python3-markdown vim-gtk racket xul-ext-noscript

# Problems

## Several kernels

`lb binary` fails if the directory `chroot/boot/` has more than one linux kernel. Have to purge all older kernels:

    dpkg --list | egrep -i --color 'linux-image|linux-headers'

    apt-get --purge remove  linux-image-4.8.0-2-amd64   linux-image-4.9.0-1-amd64

