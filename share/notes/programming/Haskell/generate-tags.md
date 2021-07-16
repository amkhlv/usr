Tags for Vim
============

Codex
-----

First install `codex`:

    cabal install codex

Then, in the project root directory:

    codex update

Use tags in vim
---------------

In `.vimrc` should be a line:

    set tags=tags;/,codex.tags;/

Then, in `vim`, pressing `Ctrl-]` navigates to the tag for the thing under cursor.

Excellent tips [are here](https://kulkarniamit.github.io/whatwhyhow/howto/use-vim-ctags.html)
