{ mkDerivation, base, containers, directory, lib
, optparse-applicative, parsec, process, split, text
}:
mkDerivation {
  pname = "amkbd";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory optparse-applicative parsec process split
    text
  ];
  homepage = "https://github.com/githubuser/amkbd#readme";
  license = lib.licenses.bsd3;
  mainProgram = "amkbd";
}
