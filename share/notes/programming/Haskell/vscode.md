# VSCode with Haskero

## Install

[Link to instructions](https://gitlab.com/vannnns/haskero/blob/master/client/doc/installation.md)

Basically:

    stack build
    stack build intero --copy-compiler-tool

Also, for the look-up to work, need to build `hoogle` indices.
Again, __in the project directory__ :

    stack hoogle

## Using

Every time after changing the `Cabal` file (__adding new package dependency__), 
click the `Targets` option on the right side of the bottom bar and then click `Validate` in the dialog that pops up. 
This forces `Intero` to restart.
