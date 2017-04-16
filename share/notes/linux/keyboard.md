Generate PDF of keyboard layout
===============================

    setxkbmap -layout us -variant altgr-intl -option nodeadkeys -print | xkbcomp - - | xkbprint -ll 3 - - | ps2pdf - > us-intl.pdf

Notice that `-ll 3` means `AltGr` pressed

