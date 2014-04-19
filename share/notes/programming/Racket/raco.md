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

(notice that the first `amkhlv` is my name on `github`, and the second is the name of the package).
