# Install package

    raco pkg install --link bystroTeX/

# Remove package

    raco pkg remove bystroTeX

(notice no trailing slash!)

# Installing from github

To install from [github](http://github.com) :

    raco pkg install github://github.com/amkhlv/amkhlv/master

Notice that the first `amkhlv` is my name on `github`, and the second is the name of the package.
Also notice that this installs the package (not planet) so it should then be called

    (require amkhlv/bystroTeX/common)

(but not `(require (planet amkhlv/bystroTeX/common))` !)

# Removing

To remove:

    raco planet remove -e amkhlv bystroTeX.plt 6 4

here the flag C<-e> means: also remove the package's distribution file from the uninstalled-package cache


# Planet (obsolete)

## Raco planet

To show the current situation:

    raco planet show

Development link:

    raco planet link amkhlv bystroTeX.plt 6 4 /home/andrei/a/git/amkhlv/bystroTeX/

If we later want to remove the development link (and do the real install), we need to __unlink__ :

    raco planet unlink amkhlv bystroTeX.plt 6 4

## Building packages

It seems that this only can be done after the package has been linked!

To prepare package from the dir, do:

    raco planet create bystroTeX/

In particular, this will generate `bystroTeX/planet-docs/manual/index.html`

To inject in the local `.racket` cache, do this:

    raco planet fileinject amkhlv bystroTeX.plt 1 1

but I __dont know why I would need it__



