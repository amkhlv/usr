Installation
============

    apt-get install  ijsgutenprint 


Printer statistics
==================

    lpstat -p
    lpstat -t

Printer options
===============

To learn printer-specific options:

    lpoptions -p $PRINTER_NAME -l

To print with some option:

    lpr -P  $PRINTER_NAME -o InputSlot=Tray1   file.pdf

Un-Pause
========

If the printer is stuck __paused__ :

    cupsenable <PRINTER_NAME>


Current jobs
============

Viewing
-------

For all currently processed jobs on the printer `ift3` :

    lpq -P ift3


Cancelling
----------

To cancel job no. 12 (job number reported by `lpq`)

    lprm 12



