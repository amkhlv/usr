#!/usr/bin/env cabal
{- cabal:
build-depends:
  base, haskell-say
-}

import HaskellSay

main :: IO ()
main = haskellSay "Hello, Haskell!"
