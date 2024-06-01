{ mkDerivation, wrapQtAppsHook, pkgs ? import <nixpkgs> {} }: 

mkDerivation {
  pname = "ioqml";
  version = "1.0";
  builder = "${pkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  src = pkgs.fetchFromGitHub {
    owner  = "amkhlv";
    repo   = "ioqml";
    hash = "sha256-+7zzjdwTXoM/Q6XDsdyouoINBjwg4lZJmNsXWZI3pV8=";
    rev = "4f70c5b52ee6798ff018ea03e58a63c62a2dd76a";
  };

  
  buildInputs = [ pkgs.pkg-config pkgs.libsForQt5.poppler pkgs.libsForQt5.qt5.qtdeclarative pkgs.libsForQt5.qt5.qtquickcontrols2 ];
  nativeBuildInputs = [ wrapQtAppsHook ]; 
}


  

