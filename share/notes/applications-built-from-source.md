
wkhtmltopdf
===========

This needs to be built in order for hyperlinks to work in created `PDF` files.

Instructions are on [their GitHub page](https://github.com/wkhtmltopdf/wkhtmltopdf/blob/master/INSTALL.md).

The command `sudo scripts/build.py setup-schroot-...`  bootstraps a Debian system into a subdirectory of `/var/chroot/` (which can be then
booted, in principle, by `systemd-nspawn`). Actually, it creates two roots: one for `amd64` and one for `i386`.

The build command itself (`scripts/build.py ...`) takes couple hours. It creates executables in `static-build/jessite-amd64/bin/` (actually
it also creates `.deb`)

CopyQ
=====

    cd CopyQ
    cmake -DCMAKE_INSTALL_PREFIX:PATH=/usr/local/opt/CopyQ
    make
    sudo make install

