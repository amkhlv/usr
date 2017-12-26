Building and installing Python
==============================

From [stack overflow](https://unix.stackexchange.com/questions/332641/how-to-install-python-3-6) :

    wget https://www.python.org/ftp/python/3.6.3/Python-3.6.3.tgz
    tar xvf Python-3.6.3.tgz
    cd Python-3.6.3
    ./configure --enable-optimizations
    make -j8
    sudo make altinstall

This only installs things like `/urs/local/bin/python3.6` so there is no chance to interfere
with the system installs

