{
  description = "HdT pdf viewer";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
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
              haskellPackages.haskell-language-server
              ghcid
              cabal-install
            ];
            inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
          };
        };
      }
    );
}
