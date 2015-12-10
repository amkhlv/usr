The resolution of virtual consoles can be set by adding the following lines to `/etc/default/grub` and then running `update-grub`:

    GRUB_GFXMODE=1024x768x32
    GRUB_GFXPAYLOAD_LINUX=keep

