{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "littledata" ./. { }) (old: {
  postInstall =
    (old.postInstall or "")
    + "\n";
})
