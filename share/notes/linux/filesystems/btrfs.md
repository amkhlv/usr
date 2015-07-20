Mount options
=============

For SSD:

    noatime,nodiratime,ssd_spread,autodefrag,compress=lzo,space_cache

Compression options are:

    compress=zlib - Better compression ratio. It's the default and safe for olders kernels.
    compress=lzo  - Faster compression.
    compress=no   - Disables compression (starting with kernel 3.6).

