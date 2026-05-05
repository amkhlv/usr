{
  description = "Clipboard transformer";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        dflt = import ./default.nix { inherit pkgs; };
      in
      {
        packages.default = dflt;
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              gtk4
              gobject-introspection
              haskellPackages.haskell-language-server
              ghcid
              cabal-install
              wl-clipboard
              pkg-config
            ];
            inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
          };
        };
      }
    );
}
