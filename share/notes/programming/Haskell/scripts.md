Haskell scripts
===============

The script file `myscript.hs` should contain the following code:

    #!/usr/bin/env cabal
    {- cabal:
    build-depends:
      base, somemore, ...
    -}


    main :: IO ()
    main = putStrLn "Hello, Haskell!"

For the lagnuage server to work, need also to create the `hie.yaml` ,  `cabal.project` and `foo.cabal` files with the following content:


    echo "packages: foo.cabal" > cabal.project

    echo "cabal-version: 3.0" > foo.cabal
    echo "name: foo" >> foo.cabal
    echo "version: 1.0.0.0" >> foo.cabal

(Ou, just symlink to `~/usr/share/Haskell/cabal.project`)
