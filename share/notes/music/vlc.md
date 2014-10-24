# Installation

If use in the console mode, install these:

    sudo apt-get install vlc-nox

However, I was only able to setup in the GUI mode:

    sudo apt-get install vlc

In any case, I think this is needed:

    sudo apt-get install vlc-plugin-pulse

# Setup

## About CD titles:

    In VLC, the CDDB retrievals are working for Audio CD's, but

      * When VLC starts for the first time, set Album Art Policy to "As soon as track is added". Or configure this in "Playlist" later.
      * Set "Show settings > All".
      * Under "Input/Codecs > Access Modules > Audio Compact Disc Digital Audio (CD-DA) Input" you must enable "Contact CDDB via the HTTP protocol"

    That is all... next time you start VLC and tell it to play your Audio CD, it will automatically retrieve and display CDDB info.

# Use

## Web interface

If started with:

    vlc  -Ihttp   ...

has web interface on [http://localhost:8080](http://localhost:8080)

## To play a collection of files in the directory, in the alphabetic order:

    find . -type f | sort | xargs vlc 

To use the browser interface instead:

    vlc -I http

This opens a server on `localhost:8080`

## To play cd:

    vlc cdda:///dev/scd0

To play cd in remote control mode (no GUI):

    vlc -Irc cdda:///dev/scd0

To play cd in ncurses mode:

    vlc -Incurses cdda:///dev/scd0
