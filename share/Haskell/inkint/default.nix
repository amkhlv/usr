{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "inkint" ./. { }) (old: {
  postInstall =
    (old.postInstall or "");
})
