Building with profiling
=======================

    stack build --profile --ghc-options=-fprof-auto-top

Executing with profiling
========================

    stack exec -- myprog-exe +RTS -p

This produces the file `myprog-exe.prof` which contains all the stats.

