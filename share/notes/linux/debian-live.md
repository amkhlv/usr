# Links

[The Manual for Stable](http://live.debian.net/manual/stable/html/live-manual.en.html)

# Installing and Configuration

    aptitude install live-build

    mkdir live-default
    cd live-default
    lb init

## Adding packages

    echo iptables-persistent >> config/package-lists/desktop.list.chroot
    echo task-xfce-desktop >> config/package-lists/desktop.list.chroot

## Adding files

### Adding files so that they are available inside the filesystem:

To include files, simply add them to your config/includes.chroot directory. This directory corresponds 
to the root directory `/` of the live system.
Example of `config/includes.chroot` :

    includes.chroot/lib/live/config/0030-user-setup
    includes.chroot/etc/iptables/rules.v4
    includes.chroot/etc/iptables/rules.v6

To put file in the __home directory__, copy it to this location:

    includes.chroot/etc/skel/


## Changing passwords and other customization

### Changing user password

On the main system, install the __source__ of the `live-config` package:

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


# Building

    lb config --distribution jessie --archive-areas "main contrib non-free" --bootappend-live "boot=live config username=andrei" --mirror-binary-security http://security.debian.org/   --mirror-chroot-security http://security.debian.org/  --security true
    lb build

