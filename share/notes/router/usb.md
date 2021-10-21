USB on router
=============

## As TTY

It seems that most of USB to TTL adaptors sold are of the type `CP210x`. 
`OpenWRT` has package: `kmod-usb-serial-cp210x` .

Moreover `OpenWRT` has `tio` package.


## As storage

We follow [instructions here](https://wiki.openwrt.org/doc/howto/extroot) :

### Needed packages

    PACKAGES="blkid block-mount kmod-usb2 kmod-usb-storage kmod-fs-ext4 kmod-scsi-core"

### Copying files to overlay

    mount /dev/sda1 /mnt ; tar -C /overlay -cvf - . | tar -C /mnt -xf - ; umount /mnt

### Mounting overlay

Instead of the usual `/etc/fstab`, `OpenWRT` uses a configuration file `/etc/config/fstab`, which
does not use the standard `fstab` syntax, and uses instead the `OpenWRT` config syntax.

[See the manual](https://wiki.openwrt.org/doc/uci/fstab)

A tentative `fstab` file is created as follows:

    block detect > /etc/config/fstab

Then edit `/etc/config/fstab` and:

1. set the mount point to: `/overlay` 

2. can use `label` instead of `uuid` 

3. set `enabled` to 1

Mounting at boot time should be enabled:

    /etc/init.d/fstab enable

The parameter `root_delay` in `/etc/config/fstab` should usually be increased to something like 15 seconds.
