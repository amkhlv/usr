Installing
==========

    git clone https://github.com/tadzik/rakudobrew ~/.rakudobrew

Then prepend `~/.rakudobrew/bin` to `PATH`, and:

    rakudobrew build moar
    rakudobrew build zef
    zef install Task::Star

Sometimes need to run:

    rakudobrew rehash

after installation: `rehash` [looks for new executables](https://www.iinteractive.com/notebook/2015/06/02/rakudobrew.html) 
in your current Rakudo installation, and creates shims for them in the Rakudobrew bin path.

Upgrading
=========

    rakudobrew self-upgrade

    rakudobrew build moar

Docs (p6doc)
============

Installing `Task::Star` installs also `p6doc` which after `rakudobrew rehash` gets on the `PATH`.


