# Raco planet

To prepare package from the dir, do:

    raco planet create bystroTeX/

To inject in the local `.racket` cache, do this:

    raco planet fileinject amkhlv bystroTeX.plt 1 1

To remove:

    raco planet remove -e amkhlv bystroTeX.plt 1 1
