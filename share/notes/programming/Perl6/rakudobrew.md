Installing
==========

    git clone https://github.com/tadzik/rakudobrew ~/.rakudobrew

Then prepend `~/.rakudobrew/bin` to `PATH`, and:

    rakudobrew build moar
    rakudobrew build zef
    zef install p6doc

Sometimes need to run:

    rakudobrew rehash

after installation: `rehash` [looks for new executables](https://www.iinteractive.com/notebook/2015/06/02/rakudobrew.html) 
in your current Rakudo installation, and creates shims for them in the Rakudobrew bin path.

Documentation
=============

There is `p6doc` executable in `~/.rakudobrew/bin/`

Upgrading
=========

    rakudobrew self-upgrade

    rakudobrew build moar



