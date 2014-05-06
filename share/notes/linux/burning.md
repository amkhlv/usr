# Disk burning using wodim

    wodim -dao linux.iso

This should work. The default -tao mode, however, does not work!

# How to write to disk from stdin

    find . -print0 | cpio -0ao | growisofs -Z /dev/dvd=/dev/fd/0

(md5 wont match)
