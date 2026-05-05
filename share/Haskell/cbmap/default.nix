{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "cbmap" ./. { }) (old: {
  executableToolDepends = (old.executableToolDepends or [ ]) ++ [
    pkgs.wrapGAppsHook4
  ];
  preFixup =
    (old.preFixup or "")
    + ''
      gappsWrapperArgs+=(
        --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.wl-clipboard ]}
      )
    '';
})
