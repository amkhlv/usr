Clipboard
=========

To set the clipboard contents:

    echo hi | termux-clipboard-set 

To get:

    termux-clipboard-get


Dial a number
=============

    termux-open-url tel:12345678

(while `termux-telephony-call` does not seem to work...)

Take a photo
============

    termux-camera-photo eraseme.jpeg

--- this happens silently, often with delay

Record sound
============

    termux-microphone-record -f eraseme.mp3

--- starts recording sound in the background

    termux-microphone-record -i

--- info about current recording process

    termux-microphone-record -q

--- stop recording


Play sound
==========

    termux-media-player play myfile.mp3
    termux-media-player info
    termux-media-player stop

WiFi scan
=========

    termux-wifi-scaninfo

Speech to text
==============

    termux-speech-to-text

This is fun !
