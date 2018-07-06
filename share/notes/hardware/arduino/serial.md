
Interacting via USB
===================

Preliminaries
-------------

To interact with `/dev/ttyACM0` the user should be in `dialout` group.

Reset problem
-------------

Arduino usually resets on starting `serial` connection. 

As explained [here](https://stackoverflow.com/questions/3918032/bash-serial-i-o-and-arduino) , have to open the connection 
(by `exec` opening a file descriptor) and keep it open:

    exec 3<> /dev/ttyACM0

    sleep 5

    stty -F /proc/$$/fd/3 cs8 9600 -ignbrk -brkint -icrnl -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh -ixon

    echo A >&3

    # blah-blah-blah
    #
    # and at the end close the file descriptor:

    exec 3>&-

The `sleep` step is crucial, because opening the serial will reset the board. Need time to wait it to reset and settle down !

Using GNU screen for testring
-----------------------------

    screen /dev/ttyACM0 9600

The transmission is keystroke-by-keystroke, no need to press `Enter`.

USB to serial adapter
=====================

Green wire should go to the RX of Arduino, and white to the TX for Arduino.
