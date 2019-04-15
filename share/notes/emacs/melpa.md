Local mirror
============

    mkdir ~/.cache/melpa
    rsync -avz --delete --progress rsync://stable.melpa.org/packages/ ~/.cache/melpa/

Then, in `.emacs` :

    (setq package-archives nil)
    (add-to-list 'package-archives '("melpa_local" . "/home/andrei/.cache/melpa/"))
    (package-initialize)


Problem with this solution
==========================

the `rsync` goes without TLS...
