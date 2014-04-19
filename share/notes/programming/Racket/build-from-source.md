Building Racket from source
===========================

From [http://www.extellisys.com/articles/racket-on-debian-wheezy](http://www.extellisys.com/articles/racket-on-debian-wheezy) :

    wget http://download.racket-lang.org/installers/6.0/racket-6.0-src-builtpkgs-unix.tgz
    tar -zxf racket-6.0-src-builtpkgs-unix.tgz
    cd racket-6.0/src
    mkdir build
    cd build
    ../configure --prefix=/usr/local/
    make
    sudo make install

