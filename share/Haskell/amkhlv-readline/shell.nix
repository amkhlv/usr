with (import <nixpkgs> {});
mkShell rec {
  buildInputs = [
              cabal-install
              haskell-language-server
              cabal2nix
              pkg-config
              zlib
              zlib.dev
              bintools
              racket
              libedit
              editline
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
