{
  description = "svg-show development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.callPackage ./default.nix { };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          package = pkgs.callPackage ./default.nix { };
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ package ];

            packages = with pkgs; [
              cargo
              rustc
              rustfmt
              clippy
              rust-analyzer
              rustPlatform.rustLibSrc
              lldb
            ];

            RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";
            GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
          };
        }
      );
    };
}
