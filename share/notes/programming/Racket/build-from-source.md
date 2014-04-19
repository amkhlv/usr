Building Racket from source
===========================

From [http://www.extellisys.com/articles/racket-on-debian-wheezy](http://www.extellisys.com/articles/racket-on-debian-wheezy) :

First navigate to the [Racket downloads page](http://racket-lang.org/download/) and choose `Unix source + build packages` :
The download button will appear, copy its `http` address. For example, if it is `http://download.racket-lang.org/installers/6.0/racket-6.0-src-builtpkgs-unix.tgz` do this:

    wget http://download.racket-lang.org/installers/6.0/racket-6.0-src-builtpkgs-unix.tgz
    tar -zxf racket-6.0-src-builtpkgs-unix.tgz
    cd racket-6.0/src
    mkdir build
    cd build
    ../configure --prefix=/usr/local/opt/racket-6.0
    make
    sudo make install

To install it under `/usr/local/` you first have to install `GNU stow` as follows:

    sudo apt-get install stow
    cd /usr/local/opt
    sudo stow racket-6.0

To later uninstall it, say:

    cd /usr/local/opt
    sudo stow -D racket-6.0

