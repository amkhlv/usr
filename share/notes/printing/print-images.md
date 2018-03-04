
Main commands
=============

First method
------------

    lpr -o scaling=100 anuencia.png

Here `scaling=100` means that the image takes 100% of the page. Maybe this is good for
images whose size is smaller than the printer page, to scale them up. I think I had a problem
with this command when printing larger images...

Second method (more reliable)
-----------------------------

First resize image using `convert -resize`

    pngtopnm tdr-300.png | pnmtops -equalpixels -nocenter | lpr 

where `tdr-300.png` is a file calculated to resolution 300dpi, which is
typically the resolution of the printer. It appears to attach image to the bottom margin,
(and maybe the right margin) on the printer. So, if need to center, pad the bottom margin of the
image (and the right margin?). There is also an option `-o page-bottom=N` where `N` is in 1/72th of an inch.
(Similarly there are `page-top=`, `page-left=` and `page-right=`)

Paper sizes in pixels and in inches
===================================

    72 dpi  = 595 X 842 pixels
    300 dpi = 2480 X 3508 pixels (This is "A4" as I know it, i.e. "210mm X 297mm @ 300 dpi")
    600 dpi = 4960 X 7016 pixels

I think the ISO standard for A4 is 210mm X 297mm (8.27 in. 11.69 in.): [A4 standard](http://www.paper-paper.com/A4-1.html)

    US Letter 	215.9mm x 279.4mm 	2550 x 3300 pixels 	300 dpi

