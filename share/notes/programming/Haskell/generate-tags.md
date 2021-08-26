Tags for Vim
============

Codex
-----

Install __globally__ `hasktags` and `codex`:

    cabal install hasktagss
    cabal install codex

Then, in the project root directory:

    hasktags --ctags .
    codex update

Use tags in vim
---------------

In `.vimrc` should be a line:

    set tags=tags;/,codex.tags;/

Then, in `vim`, pressing `Ctrl-]` navigates to the tag for the thing under cursor.

Excellent tips [are here](https://kulkarniamit.github.io/whatwhyhow/howto/use-vim-ctags.html)
