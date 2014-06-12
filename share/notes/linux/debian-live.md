# Links

[The Manual for Stable](http://live.debian.net/manual/stable/html/live-manual.en.html)

[The Manual for Unstable](http://live.debian.net/manual/unstable/html/live-manual.en.html)

# Installing

    aptitude install live-build

# Configuration

## Fresh start

    mkdir live-default
    cd live-default
    lb init
    lb config --distribution jessie

## Adding packages

    echo iptables-persistent >> config/package-lists/desktop.list.chroot
    echo task-xfce-desktop >> config/package-lists/desktop.list.chroot
    echo vim-gtk >> config/package-lists/desktop.list.chroot
    echo ntfs-3g >> config/package-lists/desktop.list.chroot
    echo screen >> config/package-lists/desktop.list.chroot
    echo gparted >> config/package-lists/desktop.list.chroot

## Adding files

### Adding files so that they are available inside the filesystem:

To include files, simply add them to your config/includes.chroot directory. This directory corresponds 
to the root directory `/` of the live system. For example, to add a file `/var/www/index.html` in the live system, use: 

Example of `config/includes.chroot` :

    includes.chroot/lib/live/config/0030-user-setup
    includes.chroot/etc/iptables/rules.v4
    includes.chroot/etc/iptables/rules.v6
    includes.chroot/etc/ssh/sshd_config
    includes.chroot/etc/ssh/ssh_config

### Just adding extra files to flashka

To include material such as documentation or videos on the medium
filesystem so that it is accessible immediately upon insertion of the
medium without booting the Live system, you can use binary local
includes. This works in a similar fashion to chroot local includes. For
example, suppose the files `~/video_demo.*` are demo videos of the live
system described by and linked to by an HTML index page. Simply copy the
material to `config/includes.binary/`

## Changing passwords and other customization

### Changing user password

Install on the main system, download the source of the `live-config` package:

    apt-get source live-config

Go to the root of the source of `live-config`, and into the directory called `components`
In that directory, find the file:

    0030-user-setup

Copy it to:

    config/includes.chroot/lib/live/config/0030-user-setup

Edit the line containing the word `_PASSWORD`; notice that it is the shadow! To convert plaintext password to the shadow hash use:

    mkpasswd -m sha-512

\--- it will ask to enter the plaintext password.
The resulting long string should be put into `config/includes.chroot/lib/live/config/0030-user-setup` as `_PASSWORD`
(Dont forget to put it into single quotation marks, because there are several dollar signs in that string!)

### Changing username

To change the default username you can simply specify it in your config:

    lb config --bootappend-live "boot=live config username=live-user"

# Building

    lb build

# Burning to USB

    dd if=binary.hybrid.iso of=${USBSTICK}
