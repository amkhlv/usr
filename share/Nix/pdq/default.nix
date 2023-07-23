{ mkDerivation, wrapQtAppsHook, pkgs ? import <nixpkgs> {} }: 

mkDerivation {
  pname = "pdq";
  version = "1.0";
  builder = "${pkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  src = pkgs.fetchFromGitHub {
    owner  = "amkhlv";
    repo   = "pdq";
    sha256 = "0aplzh3ifbaplx3ixjkk936s43wdzyqmfwlzsxdy5351pkn0iwyh";
    rev = "7f91433a7922b96aa79acfd0dd6ce22014d6c1b9";
  };

  
  buildInputs = [ pkgs.pkg-config pkgs.libsForQt5.poppler  ];
  nativeBuildInputs = [ wrapQtAppsHook ]; 
}


  

