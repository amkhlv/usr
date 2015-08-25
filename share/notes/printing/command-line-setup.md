Preparation
===========

<span style="color:red;font-weight:bold;">Remove lpr and install cups-bsd as follows:</span>

    aptitude remove lpr
    aptitude install cups-bsd

(because otherwize the commans `lpr` will not work)

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

Suppose that `lpinfo` gave us:

    drv:///hpcups.drv/hp-laserjet_p4515n.ppd HP LaserJet p4515n, hpcups 3.14.6

We then say:

    lpadmin -p myprintername -v lpd://172.16.10.13/queue -m drv:///hpcups.drv/hp-laserjet_p4515n.ppd -L location -E

Test
====

    lpr -P myprintername /usr/share/cups/data/default-testpage.pdf

