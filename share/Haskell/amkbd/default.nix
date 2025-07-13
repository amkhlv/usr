{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "amkbd" ./. { }) (old: {
  postInstall =
    (old.postInstall or "");
})
