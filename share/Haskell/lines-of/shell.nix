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
              glib
              cairo
              expat
              pcre2
              pcre.dev
              hyperscan
              xorg.libXdmcp
              xorg.libXtst
              pango
              util-linux.dev
              libselinux
              libsepol
              fribidi
              libthai
              libdatrie
              gtk3
              libxkbcommon
              libepoxy
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
