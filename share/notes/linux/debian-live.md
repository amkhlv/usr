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

Then modify the filesystem which is in `chroot/` ; install what is needed, add users etc. using:

    systemd-nspawn  -D chroot/  --network-interface=eth0  --boot

Comments:

1. Remember that to exit need to press `Ctrl-]` three times rapidly

2. the flat `--network-interface` actually steals the interface from the host; need to execute `dhclient` in the container

Finally make the ISO image:

    lb binary


# Cleanup

If something needs correction, clean up the images:

    lb clean --binary

correct the `chroot` filesystem, then `lb binary` again
