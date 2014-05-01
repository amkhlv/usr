# Raco planet

To prepare package from the dir, do:

    raco planet create bystroTeX/

To inject in the local `.racket` cache, do this:

    raco planet fileinject amkhlv bystroTeX.plt 1 1

To remove:

    raco planet remove -e amkhlv bystroTeX.plt 1 1

# Planet 2

To install:

    raco pkg install github://github.com/amkhlv/amkhlv/master

Notice that the first `amkhlv` is my name on `github`, and the second is the name of the package.
Also notice that this installs the package (not planet) so it should then be called

    (require amkhlv/bystroTeX/common)

(but not `(require (planet amkhlv/bystroTeX/common))` !)

