Preparation
===========

<span style="color:red;font-weight:bold;">Remove lpr and install cups-bsd as follows:</span>

    aptitude remove lpr
    aptitude install cups-bsd

(because otherwize the commans `lpr` will not work)

Perhaps I also might need:

    apt-get install openprinting-ppds printer-driver-all ijsgutenprint 

Useful links
============

An excellent writeup can be found [here](http://blog.tremily.us/posts/Adding_a_network_printer_with_lpadmin/)

Hewlett-Packard
===============

To install more `PPD` files for HP printers, install:

    aptitude install hplip


Finding INFO about printer
==========================

    lpinfo --make-and-model "LaserJet" -m

This will give a long list of printers which can be `grep`ped.


Setting up
==========

Laser Printer
-------------

Suppose that `lpinfo` gave us:

    drv:///hpcups.drv/hp-laserjet_p4515n.ppd HP LaserJet p4515n, hpcups 3.14.6

We then say:

    lpadmin -p myprintername -v lpd://172.16.10.13/queue -m drv:///hpcups.drv/hp-laserjet_p4515n.ppd -L IFT-3rd-floor -E

Jet Printer
-----------

Notice that home `DeskJet` type of printer is likely to support the `ipp` protocol, e.g.:

    lpadmin -p hp-home -v ipp://192.168.1.153 -m drv:///hpcups.drv/hp-deskjet_2540_series.ppd -L home -E

Sometimes the `lpinfo` does not report a `PPD` file, but only reports a `Directory` , for example:

    gutenprint.5.2://bjc-MULTIPASS-MP250/expert Canon PIXMA MP250 - CUPS+Gutenprint v5.2.12

In this case, say:

    lpadmin -p canon -v 'usb://Canon/MP250%20series?serial=91586C&interface=1' -m 'gutenprint.5.2://bjc-MULTIPASS-MP250/expert' -L IFT -E

USB printer
-----------

To obtain the list of devices, say:

    lpinfo -v

The corresponding device will go under `-v` flag.

Test
====

    lpr -P myprintername /usr/share/cups/data/default-testpage.pdf

Remove printer
==============

    lpadmin -x myprintername

