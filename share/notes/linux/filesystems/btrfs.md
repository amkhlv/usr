Mount options
=============

For SSD:

    noatime,nodiratime,ssd_spread,autodefrag,compress=lzo,space_cache

Compression options are:

    compress=zlib - Better compression ratio. It's the default and safe for olders kernels.
    compress=lzo  - Faster compression.
    compress=no   - Disables compression (starting with kernel 3.6).


Viewing free space
==================

    btrfs filesystem df /

or:

    btrfs filesystem show /


Balancing
=========

Apparently it is OK to balance the mounted system:


    btrfs balance start /

Notice that `journalctl -f` shows progress of this operation.

If this reports not enough space, try to start with lower values of the `usage` parameter:

    btrfs balance start -dusage=5 /

(Here `usage` is in percentage; the smaller is easier to do; start with 5 percent and then increase).


Scrubbing and inspecting
========================

Scrubbing
---------

    btrfs scrub start /mnt/.../

where `/mnt/.../` is where the system is mounted.

Inspecting
----------

Suppose that, during `scrub`, the `journalctl` outputs:

    error at logical 346797389472 on dev ...

Which file is that? Execute:

    btrfs ins logical-resolve -v -o 346797389472 /mnt/.../

There is a __subtlety with subvolumes__ : it seems that in order for the resolve to work, the corresponding __subvolume__ 
(the subvolume where the logical block is) must be mounted. If it is not mounted, the resolve will not work 
(and there will be message askign to mount it).
