Update
======

    pacman -Syu

Install
=======

    pacman -S package_name

Clean cache
===========

First install `pacman-contrib`:

    pacman -S pacman-contrib

Then, to clean all cache, say:

    paccache -rk 0

(Similarly, if want to keep 1 most recent update, say `paccache -rk 1`)


    
