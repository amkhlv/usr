Haskell Language Server
=======================

Installation
------------

    ghcup install hls

Sometimes need hie-bios config
------------------------------

__It seems that this step is not needed__

    stack install implicit-hie
    
    gen-hie > hie.yaml

(This file `hie.yaml` can then be edited.) 

Other people say, just have empty `hie.yaml` containing just two lines:

    cradle:
        stack:


Vim
---

Issue :CocConfig and add the following to your Coc config file.

    {
      "languageserver": {
        "haskell": {
          "command": "haskell-language-server-wrapper",
          "args": ["--lsp"],
          "rootPatterns": ["*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"],
          "filetypes": ["haskell", "lhaskell"]
        }
      }
    }

Goto-definition does not work. Need to use [tags](generate-tags.md) instead.
