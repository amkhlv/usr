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

