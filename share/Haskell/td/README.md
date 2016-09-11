Building
========

    stack build

or, to install immediately by copying the executable to `~/.local/bin/`:

    stack install

Schema files
============

The schema files are in `schemas/`.

The files `td.hs.rng` should go to `~/.config/amkhlv/`.

XML files
=========

The directory `~/.config/amkhlv/` should also contain a file called `td.hs.xml`.
It should have the following structure (confirming to `td.hs.rng`):

1. The content of the element `rngFile` should be the full path to `todolist.rng`

2. The content of each element `todolist` should be the path to the corresponding `XML` file, which should confirm to `todolist.rng`

