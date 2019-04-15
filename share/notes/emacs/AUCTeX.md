Installation
============

Download `auctex-12.1.tar.gz` from [the GNU site](https://www.gnu.org/software/auctex/download-for-unix.html)

Do `./configure` → `make` → `make install`

Also:

    aptitude install preview-latex-style

(seems like only needed to get the `.sty` file)

Then, in `.emacs` add:

    (add-to-list 'load-path "~/path/to/where/extracted/auctex-12.1/")
    (load "auctex.el" nil t t)
    (load "preview-latex.el" nil t t)





