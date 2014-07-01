# Raco planet

To show the current situation:

    raco planet show

## Package management

### Installing from github

To install from [github](http://github.com) :

    raco pkg install github://github.com/amkhlv/amkhlv/master

Notice that the first `amkhlv` is my name on `github`, and the second is the name of the package.
Also notice that this installs the package (not planet) so it should then be called

    (require amkhlv/bystroTeX/common)

(but not `(require (planet amkhlv/bystroTeX/common))` !)

### Removing

To remove:

    raco planet remove -e amkhlv bystroTeX.plt 6 4

here the flag C<-e> means: also remove the package's distribution file from the uninstalled-package cache

### Show

    raco planet show

shows the current situation. Notice that in `raco planet remove -e` , the rest arguments after `-e` can
be simply copy-pasted from the output of `raco planet show`. 

### Development links for Planet

First, remove the package if it was already installed. Then:

    raco planet link amkhlv bystroTeX.plt 6 4 /home/andrei/a/git/amkhlv/bystroTeX/

If we later want to remove the development link (and do the real install), we need to __unlink__ :

    raco planet unlink amkhlv bystroTeX.plt 6 4

In any case, always use `raco planet show` to see the situation.

## Building packages

To prepare package from the dir, do:

    raco planet create bystroTeX/

To inject in the local `.racket` cache, do this:

    raco planet fileinject amkhlv bystroTeX.plt 1 1

# Planet 2

To install:




