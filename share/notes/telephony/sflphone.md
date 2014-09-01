Downloading and building
========================

From `git` :

    git clone https://gerrit-sflphone.savoirfairelinux.com/sflphone
    git checkout master

Excellent built instructions for Debian (including dependencies):

1. [Install build dependencies](https://projects.savoirfairelinux.com/projects/sflphone/wiki/Debian-based)

2. Then build as described [here](https://projects.savoirfairelinux.com/projects/sflphone/wiki/How_to_build), more specifically:


More specifically, we first build libraries:

    cd daemon/contrib/
    mkdir native
    cd native
    ../bootstrap
    make

Then, from the __root__ of the git:

    cd daemon
    ./autogen.sh
    ./configure --prefix=/usr/local/opt/sflphone/
    make
    sudo make install

Again, from the __root__ of the git:

    cd gnome
    ./autogen.sh
    ./configure --prefix=/usr/local/opt/sflphone/
    make
    sudo make install


Setup
=====



