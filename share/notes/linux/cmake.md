How to use
==========

The __root of the project__ is the directory containing the file called `CMakeLists.txt`

It is __not__ a good practice to build in the root directory. Usually, one creates a subdirectory of the root directory called `build` and
builds there:

    mkdir build
    cd build
    cmake -DCMAKE_INSTALL_PREFIX:PATH=/usr/local/opt/programname ../

