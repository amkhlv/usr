Uploading from the command line
===============================

First install `arduino-mk` :

    sudo aptitude install arduino-mk

Create a directory `myproject` and a file `myproject/Makefile` with the following content:

    ARDUINO_DIR  = /usr/share/arduino
    BOARD_TAG    = uno
    ARDUINO_PORT = /dev/ttyACM0

    include /usr/share/arduino/Arduino.mk

The `.ino` file should be in the same folder. The name of the `.ino` file does not matter. 

Then:

    make

    make upload

The user should be in the `dialout` group.

How to press the button
=======================

`Mini Pro` has a small "reset"-type button. When uploading:

1. press it and keep it pressed
2. type `make upload` and press `Enter`, 
3. release the reset button after 1 second
