# Build Emacs from source

## Installing dependencies

    aptitude install libc6-dev libjpeg62-turbo libncurses5-dev libpng-dev libtiff5-dev libgif-dev xaw3dg-dev zlib1g-dev libx11-dev   libgtk-3-dev libwebkit2gtk-4.0-dev  librsvg2-dev
    
## Building

    ./configure --with-xwidgets --with-x-toolkit=gtk3  --prefix=$HOME/.local/share/emacs

    make
    
    make install prefix=$HOME/.local/share/emacs

Do __not__ use `--with-cairo` , it is too experimental.
